package gama
package internal
package frontend

import scala.collection.mutable.{HashMap=>HMap, HashSet=>HSet, ListBuffer=>ListB}

object TyperWidthInferer {
  case class InferenceSolution(inferredModule: ElaboratedModule, unknownsFound: Int, unknownParts: Int, solvedParts: Int)
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
    RefExtract -> INFERRED, trivially by forcing based on lp and rp

    RefExprERROR -> Ignored, trivially

    * bypassing is required since the rType of these may still contain uninferred elements, thus
      do not want a constraint that is dependent on these computed types since they will not be
      part of the constraint graph
  */
  def infer(target: ElaboratedModule): InferenceSolution = {
    val unknownTable = HMap.empty[ExprHW, HSet[TypeTrace]]
    val constraints = new ConstraintTable
    val dealiaser = new Dealiaser

    def addUnknown(expr: ExprHW, unknown: TypeTrace): Unit = {
      unknownTable.getOrElseUpdate(expr, HSet.empty[TypeTrace]) += unknown
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
          case RegDecl(_,_) | ConstDecl(_,_) | AliasDecl(_,_) |
               ConnectStmt(_,_,_) | BiConnectStmt(_,_,_)
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
    // First, the constraint builders
    object ConstrainFrom extends ConstraintBuilder(constraints) {
      def buildConstraints(terms: Iterable[WidthConstraint]): Iterable[WidthConstraint] = terms
      // Just build a >= constraint from each term
    }
    object ConstrainAdd2 extends ConstraintBuilder(constraints) {
      def buildConstraints(terms: Iterable[WidthConstraint]): Iterable[WidthConstraint] = {
        require(terms.size==2, "Internal Error from malformed AST: Type Linked Scan failure")
        Seq(WidthAdd(terms.toVector))
      }
    }
    object ConstrainIncr extends ConstraintBuilder(constraints) {
      def buildConstraints(terms: Iterable[WidthConstraint]): Iterable[WidthConstraint] = 
        terms.map( term => WidthAdd(Vector(term, WidthLit(1))) )
    }
    object ConstrainLShft extends ConstraintBuilder(constraints) {
      def buildConstraints(terms: Iterable[WidthConstraint]): Iterable[WidthConstraint] = {
        require(terms.size==2, "Internal Error from malformed AST: Type Linked Scan failure")
        Seq(WidthAdd(Vector(terms.head,WidthBitM(terms.last))))
      }
    }
    case class ForceWidth(width: Int) extends ConstraintBuilder(constraints) {
      def buildConstraints(terms: Iterable[WidthConstraint]): Iterable[WidthConstraint] =
        Seq(FORCEWidthLit(width))
    }
    // Look at all expression definitions where an expression holds an unknown
    unknownTable.keys.foreach(expr => expr match {
        // TODO: Actually look at op...
      case ExprUnary(op, target, _) => op match {
        case OpIDENT | OpToUInt | OpAsUInt | OpAsSInt | OpNot =>
          ConstrainFrom.start(expr, Seq(target))
        case OpToSInt => ConstrainIncr.start(expr, Seq(target)) // add 1 bit
        case OpXorRed => ForceWidth(1).start(expr, None)
      }
      case ExprBinary(op, left, right, _) => op match {
        case OpPlus | OpSubt | OpAnd | OpOr | OpXor | OpPadTo => // max(l, r)
          ConstrainFrom.start(expr, Seq(left, right))
        case OpMult | OpCat   => // l + r
          ConstrainAdd2.start(expr, Seq(left, right))
        case OpDiv  | OpRShft => // l
          ConstrainFrom.start(expr, Seq(left))
        case OpMod   => // r?
          ConstrainFrom.start(expr, Seq(right))
        case OpLShft => // l + max(r) = l + (2^r-1) (could be huge)
          ConstrainLShft.start(expr, Seq(left, right))
        case OpEqual | OpNotEq | OpLess | OpLeEq | OpGrt | OpGrEq =>
          ForceWidth(1).start(expr, None)
      } 
      case ExprMux(_, tc, fc, _) => ConstrainFrom.start(expr, Seq(tc, fc))
      case RefExtract(source, lp, rp, _) => ForceWidth(lp-rp+1).start(expr, None)

      case RefSymbol(_,_,_) => // RefSymbols must be constrained by commands
      case _ => ??? // No other expressions are directly inferred
    })
    // Look at all constraining commands
    // TODO: quickly skip ones that are irrelevant
    constrainingCmds.foreach(cmd => cmd match {
      case RegDecl(symbol, Some((_,rval))) => ConstrainFrom.start(symbol, Seq(rval))
      case RegDecl(symbol, None) => 
      case ConstDecl(symbol, expr) => ConstrainFrom.start(symbol, Seq(expr))
      case AliasDecl(symbol, ref)  => ConstrainFrom.start(symbol, Seq(ref)) // treat like ConstDecl
      case ConnectStmt(sink, source, details) =>
        ConstrainFrom.startguided(details, dealiaser.dealias(sink), Seq(source))
      case BiConnectStmt(left, right, details) => 
        ConstrainFrom.startbiguided(details, dealiaser.dealias(left), dealiaser.dealias(right))
      case _ => ??? // These shouldn't show up as the others are not constraining commands
    })

    // STEP 3: CONSTRAINT SOLVING
    val solution = constraints.solve

    // STEP 4: BUILD TYPE REPLACEMENT TABLE
    val typeReplaceTable: Map[ExprHW, TypeHW] = unknownTable.toMap.map({case (uExpr, uTTs) => {
      val newTypeHW: TypeHW = uTTs.foldLeft(uExpr.rType)((oldTypeHW: TypeHW, tt: TypeTrace) => {
        val newWidth = solution.lookup(tt)
        val replacer: TypeHW=>TypeHW = (a) => a match {
          case PrimitiveNode(UBits(_)) => PrimitiveNode(UBits(newWidth)) 
          case PrimitiveNode(SBits(_)) => PrimitiveNode(SBits(newWidth))
          // PrimitivePort widths are assumed to be known
          case _ => throw new Exception(s"Internal Error: Path terminated at $a")
        }
        TypeTrace.remake(oldTypeHW, tt.toList.reverse, replacer)
      })
      (uExpr, newTypeHW)
    }})

    // STEP 5: WALK THE TREE REPLACING TYPES FROM THE TABLE
//          case ExprUnary(_,_,_) | ExprBinary(_,_,_,_) | ExprMux(_,_,_,_) |
 //              RefSymbol(_,_,_) | RefExtract(_,_,_,_)
    object Transformer extends ExprTransformTree {
      def getNewType(in: ExprHW): TypeHW = typeReplaceTable.get(in).getOrElse(in.rType)

      override def transform(symbol: RefSymbol): RefSymbol = symbol.copy(rType=getNewType(symbol))
      override def transform(ref: RefHW): RefHW  = ref match {
        case RefExtract(source, lp, rp, rType) =>
             RefExtract(transform(source), lp, rp, getNewType(ref))
        case _ => super.transform(ref)
      }
      override def transform(expr: ExprHW): ExprHW = expr match {
        case ExprUnary(op, target, rType) =>
             ExprUnary(op, transform(target), getNewType(expr))
        case ExprBinary(op, left, right, rType) =>
             ExprBinary(op, transform(left), transform(right), getNewType(expr))
        case ExprMux(cond, tc, fc, rType) =>
             ExprMux(transform(cond), transform(tc), transform(fc), getNewType(expr))
        case _ => super.transform(expr)
      }
    }
    val pathsUnknown = unknownTable.values.map(_.size).sum
    InferenceSolution(ElaboratedModule(target.io, Transformer.transform(target.body)),
                      unknownTable.size, pathsUnknown, solution.pathsSolved)
  }

/*
    // DEBUGGING
    val dprint = IRReader.Colorful
    println("UNKNOWNS")
    unknownTable.foreach({case (k,tts) => {
      println(s"${dprint.parseExpr(k)}: ${Console.GREEN}${dprint.parseType(k.rType)}${Console.RESET}")
      tts.foreach(tt => constraints.table.getOrElse(tt, Set.empty).foreach(c =>
        println(s"${dprint.parseTT(tt)} >= ${parseConstraint(c)}")
      ))
      println("")
    }})
    
    //println("CONSTRAINTS")
    //constraintTable.foreach({case (k,vs) => vs.foreach(v => println(s"${dprint.parseTT(k)} : ${parseConstraint(v)}"))})
  
    def parseConstraint(c: WidthConstraint): String = c match {
      case WidthLit(lit)      => s"$lit"
      case WidthRef(ref)      => s"${dprint.parseTT(ref)}"
      case WidthAdd(terms)    =>  terms.map(parseConstraint(_)) mkString("(","+",")")
      case WidthMax(consts)   => consts.map(parseConstraint(_)) mkString("max(",", ", ")")
      case WidthBitM(const)   => s"(2^${parseConstraint(const)}-1)"
      case FORCEWidthLit(lit) => s"== FORCED: $lit"
    }
*/  

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

  abstract class ConstraintBuilder(constraintTable: ConstraintTable) extends LinkedTypeScanTree {
    def buildWidthTerm(width: Option[Int], path: TypeTrace): WidthConstraint = 
      width.map(WidthLit(_)).getOrElse(WidthRef(path))

    def leafwork(leader: PrimitiveTypeHW, leadPath: TypeTrace,
                 followers: Iterable[Tuple2[PrimitiveTypeHW, TypeTrace]]) =
      for(lwidth <- getWidth(leader) if lwidth.isEmpty) { // No work to be done if width known
        val widthterms = for {
          (fPType, fPath) <- followers
          fwidth <- getWidth(fPType)
          widthterm = buildWidthTerm(fwidth, fPath)
        } yield widthterm    //} yield (fwidth, fPath): Tuple2[Option[Int], TypeTrace]

        buildConstraints(widthterms).foreach(constraint => constraintTable.add(leadPath, constraint))
      }

    def buildConstraints(terms: Iterable[WidthConstraint]): Iterable[WidthConstraint]
  }
}

