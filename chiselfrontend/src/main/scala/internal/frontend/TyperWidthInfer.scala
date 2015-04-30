package gama
package internal
package frontend

import scala.collection.mutable.{HashMap=>HMap, HashSet=>HSet, ListBuffer=>ListB}

object TyperWidthInferer {
  sealed trait Constraint
  case class GEqLit(lb: Int) extends Constraint
  case class GEqTT(lb: TypeTrace) extends Constraint

  def infer(target: ElaboratedModule): ElaboratedModule = {
    // 1. Build up table of uninferred widths and associated constraints
    //   Store as: Map[ExprHW -> Set[TypeTrace]], Map[TypeTrace -> Set[Constraints]]
    //   Constraints: >=Int, >=TypeTrace, >=(Op,TypeTrace,TypeTrace)
    //  -> Will have to walk the entire tree, particularly expressions and connections
    //      need to de-alias Expr that don't register own type (e.g. RefVIndex)
    //  -> Maybe reasonable to do a quick walk to populate first map so can skip detailed walks
    //       e.g.
    //  -> Also, will want to ensure AliasDecl is carefully handled...
    //     a. AliasDecl can act like ConstDecl where symbol constrained by ref
    //     b. HOWEVER: for ConnectStmt.sink, BiConnectStmt.left,right, constraints must be built off de-aliased
    //        Thus, will need to build de-aliasing table: Map[RefSymbol -> RefHW]
    // 2. Resolve the constraints in the second map to make result Map[TypeTrace -> Int] (or ->Option[Int]?)
    // 3. Build replacement Map[ExprHW -> ExprHW] by using first map and result map
    //  -> Can be clever and decompose the TypeTrace, group up constituent parts, and then do cloning
/*
  Summary of how each expression is inferred
    ExprUnary  -> INFERRED based on op
    ExprBinary -> INFERRED based on op
    ExprMux    -> INFERRED by type is constrained to be >= tc and fc
    ExprLit    -> ignored since literals come fully inferred

    RefSymbol  -> INFERRED based on use in commands
    RefIO      -> ignored since IOs come fully inferred
    RefMSelect -> ignored since memories come fully inferred
    RefVIndex  -> ignored since type computed on source, BYPASSING REQUIRED*
    RefVSelect -> ignored since type computed on source, BYPASSING REQUIRED*
    RefTLookup -> ignored since type computed on source, BYPASSING REQUIRED*
    RefExtract -> pseudo-INFERRED, is ??? for now as these come fully inferred

    RefExprERROR -> Ignored, trivially

    * bypassing is required since the rType of these may still contain uninferred elements, thus
      do not want a constraint that is dependent on these computed types since they will not be
      part of the constraint graph
*/

    val unknownTable = HMap.empty[ExprHW, HSet[TypeTrace]]
    val constraintTable = HMap.empty[TypeTrace, HSet[Constraint]]
    val dealiaser = new Dealiaser

    def addUnknown(expr: ExprHW, unknown: TypeTrace): Unit = {
      unknownTable.getOrElseUpdate(expr, HSet.empty[TypeTrace]) += unknown
    }
    def addConstraint(unknown: TypeTrace, constraint: Constraint): Unit = {
      constraintTable.getOrElseUpdate(unknown, HSet.empty[Constraint]) += constraint
    }

    val constrainingCmds = ListB.empty[CmdHW]

    // STEP 1. SCAN
    //  Find all unknowns, aliases, and constraining statements
    // TODO: Can have set of checked nodes and just not check those redundantly
    // Do not need to bother dealiasing when just finding items, only when establishing constraints
    def findUnknowns(searching: TypeHW, path: TypeTrace, root: ExprHW): Unit = searching match {
      case PrimitiveNode(storage) => checkUnknownStatus(storage, path, root)
      case PrimitivePort(_ ,_) => // These are always ok
      case TupleHW(fields) => fields.foreach({case (field, eType) => findUnknowns(eType, TTField(path, field), root)})
      case VecHW(_, eType) => findUnknowns(eType, TTIndexALL(path), root)
      case TypeHWUNKNOWN => ??? // This shouldn't show up
    }
    def checkUnknownStatus(searching: NodeStore, path: TypeTrace, root: ExprHW): Unit = searching match {
      case rb: RawBits => {if(rb.width.isEmpty) addUnknown(root, path)}
    }
    val prescanner = new ExprScanTree {
      override def scan(cmd: CmdHW): Unit = {
        cmd match {
          case AliasDecl(symbol, ref) => dealiaser.addAlias(symbol, ref)
          case _ =>
        }
        cmd match {
          case ConstDecl(_,_) | AliasDecl(_,_) | ConnectStmt(_,_,_) | BiConnectStmt(_,_,_)
            => constrainingCmds += cmd
          case _ =>
        }
        super.scan(cmd)
      }
      override def scan(expr: ExprHW): Unit = {
        expr match {
          case ExprUnary(_,_,_) | ExprBinary(_,_,_,_) | ExprMux(_,_,_,_) |
               RefSymbol(_,_,_) | RefExtract(_,_,_,_)
                => findUnknowns(expr.rType, TTStart(expr), expr)
          case _ =>
        }
        super.scan(expr)
      }
    }
    prescanner.scan(target.body)

    // STEP 2: CONSTRAINT CONSTRUCTION
    //  Find all constraints from expressions with unknowns and constraining commands
    unknownTable.keys.foreach(expr => expr match {
        // TODO: Actually look at op...
      case ExprUnary(op, target, _) => {
        ConstrainFrom.start(expr, Seq(target))
      }
      case ExprBinary(op, left, right, _) => {
        ConstrainFrom.start(expr, Seq(left, right))
      } 
      case ExprMux(_, tc, fc, _) => ConstrainFrom.start(expr, Seq(tc, fc))
      case RefExtract(source, lp, rp, _) => ??? // this may be skippable for now...

      case RefSymbol(_,_,_) => // RefSymbols must be constrained by commands
      case _ => // No other expressions are directly inferred
    })
    // TODO: quickly skip ones that are irrelevant
    constrainingCmds.foreach(cmd => cmd match {
      case ConstDecl(symbol, expr) => ConstrainFrom.start(symbol, Seq(expr))
      case AliasDecl(symbol, ref)  => ConstrainFrom.start(symbol, Seq(ref)) // treat like ConstDecl
      case ConnectStmt(sink, source, details) => {
        ConstrainFrom.startguided(details, dealiaser.dealias(sink), Seq(source))
      }
        // needs dealiasing of both forms
      case BiConnectStmt(sink, source, details) => ??? // needs dealiasing of both forms
      case _ => ??? // These shouldn't show up as the others are not constraining commands
    })

    object ConstrainFrom extends WidthScanTree {
      def widthwork(leadPath: TypeTrace, followers: Iterable[Tuple2[Option[Int], TypeTrace]]) =
        for ((fwidth, fPath) <- followers) fwidth match {
            case None    => addConstraint(leadPath, GEqTT(fPath))
            case Some(w) => addConstraint(leadPath, GEqLit(w))
        }
    }

    // DEBUGGING
    val dprint = IRReader.Colorful
    println("UNKNOWNS")
    unknownTable.foreach({case (k,v) => println(s"${dprint.parseExpr(k)}: ${Console.GREEN}${dprint.parseType(k.rType)}${Console.RESET}")})
    println("CONSTRAINING COMMANDS")
    constrainingCmds.foreach(v => println(dprint.parseCmdHW(v)))
    println("CONSTRAINTS")
    constraintTable.foreach({case (k,vs) => vs.foreach(v => println(s"${dprint.parseTT(k)} : ${parseConstraint(v)}"))})
    def parseConstraint(c: Constraint): String = c match {
      case GEqLit(lb) => s">= $lb"
      case GEqTT(lb)  => s">= ${dprint.parseTT(lb)}"
    }
    
    target
  }

    // Similar to aliasing from AliasDecl: since RefVIndex/VSelect/TLookup will not show up with
    //   uninferred types, will need to jump past them so we don't build a type constraint on them
    // This is because those 3 references have computed types
  def bypassCompRef(in: ExprHW): Tuple2[TypeHW, TypeTrace] = in match {
    case RefVIndex(source, _)      => {
      val res = bypassCompRef(source)
      (in.rType, TTIndexALL(res._2))
    }
    case RefVSelect(source, _)     => {
      val res = bypassCompRef(source)
      (in.rType, TTIndexALL(res._2))
    }
    case RefTLookup(source, field) => {
      val res = bypassCompRef(source)
      (in.rType, TTField(res._2, field))
    }
    case _ => (in.rType, TTStart(in))
  }

}

trait WidthScanTree extends LinkedTypeScanTree {
  override def start(leader: ExprHW, followers: Iterable[ExprHW]): Unit = {
    val (ltype, lpath) = TyperWidthInferer.bypassCompRef(leader)
    val newf = followers.map(TyperWidthInferer.bypassCompRef(_))
    pathscan(ltype, lpath, newf)
  }
  def leafwork(leader: PrimitiveTypeHW, leadPath: TypeTrace,
               followers: Iterable[Tuple2[PrimitiveTypeHW, TypeTrace]]) =
    for(lwidth <- getWidth(leader) if lwidth.isEmpty) { // No work to be done if width known
      val newfollowers = for {
        (fPType, fPath) <- followers
        fwidth <- getWidth(fPType)
      } yield (fwidth, fPath): Tuple2[Option[Int], TypeTrace]
      widthwork(leadPath, newfollowers)
    }

  def widthwork(leadPath: TypeTrace, followers: Iterable[Tuple2[Option[Int], TypeTrace]]): Unit
}
