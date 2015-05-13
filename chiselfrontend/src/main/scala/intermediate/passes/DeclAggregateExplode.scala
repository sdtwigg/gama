package gama
package intermediate
package passes

import scala.collection.mutable.{HashMap=>HMap, HashSet=>HSet, ListBuffer=>ListB}

object DeclAggregateExplode extends GamaPass {
  val name = "DeclAggregateExplode"
  // WILL NOT ADJUST ANY RefIO, this must be done separately!
  // TODO: Assumes ExpandVSelect, AggregateConnectExplode already run
  // TODO: Uses DistributeRef
  sealed trait MemPathTrace
  case class MPTStart(origin: MemDesc) extends MemPathTrace
  case class MPTField(previous: MemPathTrace, field: String) extends MemPathTrace
  case class MPTSelectOne(previous: MemPathTrace, index: Int) extends MemPathTrace

  def transform(target: ElaboratedModule): ElaboratedModule = {
    // Need these in order to properly split up aggregaes
    val symbolGen = new AppendableRefSymbolTable(target)
    val memDescGen = new AppendableMemDescTable(target)

    val path2sym = HMap.empty[PathTrace, RefSymbol]
    val sym2path = HMap.empty[RefSymbol, PathTrace]
      // PathTrace required so notes ignored upon lookup in the table
      // First one is used for actual replacement pass
      // Second one is for building a full replacement list
    val deadSymbols = HSet.empty[RefSymbol]
      // If any of these are seen and thus not replaced, an error happened
    // Similarly for memories
    val path2mem = HMap.empty[MemPathTrace, MemDesc]
    val mem2path = HMap.empty[MemDesc, MemPathTrace]
    val deadMems = HSet.empty[MemDesc]
    
    // STEP 1: Explode all Decl into primitive types
    def explodeCreatesRefSymbol(creator: CreatesRefSymbol): Iterable[CmdHW] = creator match {
      case WireDecl(_, note) =>
        explodeSymbol(creator, (nsym, _) => WireDecl(nsym, note))
      case RegDecl(_, reset, note) =>
        explodeSymbol(creator, (nsym, ewrap)=>RegDecl(nsym, reset.map({case (ren, rv) => (ren, ewrap(rv))}), note))
      case ConstDecl(_, expr, note) => 
        explodeSymbol(creator, (nsym, ewrap) => ConstDecl(nsym, ewrap(expr), note))
      case AliasDecl(_, ref, note) =>
        explodeSymbol(creator, (nsym, ewrap) => ConstDecl(nsym, ewrap(ref), note))
    }
    def explodeSymbol(target: CreatesRefSymbol,
                      cmdbuilder: (RefSymbol,ExprHW=>RefHW)=>CreatesRefSymbol): Iterable[CmdHW] =
      // The builder receives 2 arguments: the new, closer-to-primitive ref symbol
      //   and a function that converts an expression of the greater agg type to the closer-to-primitive type
      //   The second argument is so Reg reset, Const expr, and Alias ref can all be addressed
      // The build must return a new command that creates a RefSymbol
      target.symbol.rType match {
        case _: PrimitiveTypeHW => (Some( target ))
        case TupleHW(fields) => {
          deadSymbols += target.symbol // old target being exploded to invalidate its symbol
          fields.toSeq.sortBy(_._1).flatMap({case (field, eType) => {
            // First, build the new command
            val exprbuilder: ExprHW=>RefHW = expr => RefTLookup(expr, field, passNote)
            val newsymident = target.symbol.identifier.getOrElse("") + "$" + field
            val newcmdbuilder: Int=>CreatesRefSymbol = sid => {
              val newsym = RefSymbol(sid, Some(newsymident), eType, target.symbol.note)
              cmdbuilder(newsym, exprbuilder)
            }  
            val newcmd = symbolGen.grantNewSymbol(newcmdbuilder)
            // Now, build reference replacement table entries for it
            val oldPath: PathTrace = sym2path.getOrElse(target.symbol, PTStart(target.symbol))
            val newPath = PTField(oldPath, field)
            val newSym = newcmd.symbol
            path2sym += newPath -> newSym
            sym2path += newSym -> newPath
            // Finally, act recursively
            explodeCreatesRefSymbol(newcmd)
          }})
        }
        case VecHW(depth, eType) => {
          deadSymbols += target.symbol // old target being exploded to invalidate its symbol
          (0 until depth).flatMap(idx => {
            // First, build the new command
            val exprbuilder: ExprHW=>RefHW = expr => RefVIndex(expr, idx, passNote)
            val newsymident = target.symbol.identifier.getOrElse("") + "$" + idx
            val newcmdbuilder: Int=>CreatesRefSymbol = sid => {
              val newsym = RefSymbol(sid, Some(newsymident), eType, target.symbol.note)
              cmdbuilder(newsym, exprbuilder )
            }  
            val newcmd = symbolGen.grantNewSymbol(newcmdbuilder)
            // Now, build reference replacement table entries for it
            val oldPath: PathTrace = sym2path.getOrElse(target.symbol, PTStart(target.symbol))
            val newPath = PTSelectOne(oldPath, idx)
            val newSym = newcmd.symbol
            path2sym += newPath -> newSym
            sym2path += newSym -> newPath
            // Finally, act recursively
            explodeCreatesRefSymbol(newcmd)
          })
        }
        case TypeHWUNKNOWN =>
          Some( CmdERROR(s"Unknown Type encountered during $name", target.note ) )
      }
    def explodeMemDecl(oldcmd: MemDecl): Iterable[CmdHW] = oldcmd.desc.mType match {
      case _: PrimitiveTypeHW => (Some( oldcmd ))
      case TupleHW(fields) => {
        deadMems += oldcmd.desc // old target being exploded to invalidate its symbol
        fields.toSeq.sortBy(_._1).flatMap({case (field, eType) => {
          val newident = oldcmd.desc.identifier.getOrElse("") + "$" + field
          val newcmd = memDescGen.grantNewMem(memid =>
            MemDecl(MemDesc(memid, Some(newident), oldcmd.desc.depth, eType), oldcmd.note)
          )
          // Now, build reference replacement table entries for it
          val oldPath: MemPathTrace = mem2path.getOrElse(oldcmd.desc, MPTStart(oldcmd.desc))
          val newPath = MPTField(oldPath, field)
          val newDesc = newcmd.desc
          path2mem += newPath -> newDesc
          mem2path += newDesc -> newPath
          // Finally, act recursively
          explodeMemDecl(newcmd)
        }})
      }
      case VecHW(depth, eType) => {
        deadMems += oldcmd.desc // old target being exploded to invalidate its symbol
        (0 until depth).flatMap(idx => {
          val newident = oldcmd.desc.identifier.getOrElse("") + "$" + idx
          val newcmd = memDescGen.grantNewMem(memid =>
            MemDecl(MemDesc(memid, Some(newident), oldcmd.desc.depth, eType), oldcmd.note)
          )
          // Now, build reference replacement table entries for it
          val oldPath: MemPathTrace = mem2path.getOrElse(oldcmd.desc, MPTStart(oldcmd.desc))
          val newPath = MPTSelectOne(oldPath, idx)
          val newDesc = newcmd.desc
          path2mem += newPath -> newDesc
          mem2path += newDesc -> newPath
          // Finally, act recursively
          explodeMemDecl(newcmd)
        })
      }
      case TypeHWUNKNOWN =>
        Some( CmdERROR(s"Unknown Type encountered during $name", oldcmd.note ) )
    }
    def explodeMemWrite(oldcmd: MemWrite): Iterable[CmdHW] = {
      val exprIdentity = (expr: ExprHW) => expr
      // Input: outer mem type, current path to this part
      // Output: flattened element types (only primitive), trace from outer, builder for new source
      def flattenMemType(outerType: TypeHW, outerTrace: MemPathTrace
        ): Iterable[(PrimitiveNode, MemPathTrace, ExprHW=>ExprHW)] = outerType match {
        case root @ PrimitiveNode(_) =>
          Some(( root, outerTrace, exprIdentity ))

        case VecHW(depth, elemType) => for {
          idx <- 0 until depth
          aggTrace   = MPTSelectOne(outerTrace, idx)
          // Now, handle inner eType
          (eType: PrimitiveNode, newTrace, eBuilder) <- flattenMemType(elemType, aggTrace)
          aggBuilder = (expr: ExprHW) => eBuilder(RefVIndex(expr, idx, passNote))
        } yield (eType, newTrace, aggBuilder)

        case TupleHW(fields) => for {
          (field, elemType) <- fields.toSeq.sortBy(_._1)
          aggTrace   = MPTField(outerTrace, field)
          // Now, handle inner eType
          (eType: PrimitiveNode, newTrace, eBuilder) <- flattenMemType(elemType, aggTrace)
          aggBuilder = (expr: ExprHW) => eBuilder(RefTLookup(expr, field, passNote))
        } yield (eType, newTrace, aggBuilder)

        case PrimitivePort(_,_) | TypeHWUNKNOWN =>
          throw new Exception(s"Catastrophic error: Bad Mem type during $name") 
      }

      flattenMemType(oldcmd.desc.mType, MPTStart(oldcmd.desc)).map({case (_, oldPath, newBuilder) => {
        path2mem.get(oldPath).map(newMem =>
          MemWrite(newMem, oldcmd.selector, newBuilder(oldcmd.source), oldcmd.mask, oldcmd.note)
        ).getOrElse(CmdERROR(s"Lost memory reference during $name", passNote))
      }})
    }
    object DeclExplodeTransformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW): Iterable[CmdHW] = cmd match {
        case creator: CreatesRefSymbol => (explodeCreatesRefSymbol(creator))
        case memD @ MemDecl(_,_) => explodeMemDecl(memD)
        case memW @ MemWrite(_,_,_,_,_) => explodeMemWrite(memW)
        case _ => super.multiTransform(cmd)
      }
    }
    val explodedDecl = DeclExplodeTransformer.transform(target)

    // STEP 2: Ensure all aggregate sub-references distributed up as much as possible
    val smashedTarget = DistributeRef.transform(explodedDecl)

    // STEP 3:
    //   Replace references to Aggregate Symbols using refines with references to Primitive Symbols
    //   Replace references to Aggregate Mems using refines with references to Primitive Mems
    object RefReplaceTransformer extends ExprTransformTreeFullSegregation {
      sealed trait FoundPath
      case class FoundRefSymbol(path: PathTrace) extends FoundPath
      case class FoundMemSelect(path: MemPathTrace, root: RefMSelect) extends FoundPath

      def makePath(expr: ExprHW): Option[FoundPath] = expr match {
        case rootMS @ RefMSelect(mem,_,_) => Some(FoundMemSelect(MPTStart(mem), rootMS))
        case symbol @ RefSymbol(_,_,_,_)  => Some(FoundRefSymbol(PTStart(symbol)))

        case RefVIndex(source, index, _) => makePath(source).map{
          case FoundRefSymbol(path)       => FoundRefSymbol( PTSelectOne(path, index))
          case FoundMemSelect(path, root) => FoundMemSelect(MPTSelectOne(path, index), root) 
        }
        case RefTLookup(source, field, _) => makePath(source).map{
          case FoundRefSymbol(path)       => FoundRefSymbol( PTField(path, field))
          case FoundMemSelect(path, root) => FoundMemSelect(MPTField(path, field), root) 
        }

        case _ => None
      }
      override def transform(ref: RefHW): RefHW  = ref match {
        case RefVIndex(_,_,_) | RefTLookup(_,_,_) => makePath(ref).flatMap({
          case FoundRefSymbol(path)       => path2sym.get(path)
          case FoundMemSelect(path, root) => path2mem.get(path).map(RefMSelect(_, root.selector, root.note))
        }).getOrElse(ref)

        case _  => super.transform(ref)
      }
    }
    val consistent = RefReplaceTransformer.transform(smashedTarget)

    // STEP 4: Make sure no references to dead symbols persist in the tree
    object DeadCheckTransformer extends ExprTransformTreeFullSegregation {
      override def transform(cmd: CmdHW): CmdHW = cmd match {
        case MemWrite(desc,_,_,_,_) =>
          if(deadMems(desc)) CmdERROR(s"Dead memory write not replaced during $name", passNote)
          else super.transform(cmd)
        case _ => super.transform(cmd)
      }
      override def transform(ref: RefHW): RefHW = ref match {
        case symbol @ RefSymbol(_,_,_,_) =>
          if(deadSymbols(symbol)) RefExprERROR(s"Dead symbol not replaced during $name") 
          else symbol
        case rootMS @ RefMSelect(memDesc,_,_) =>
          if(deadMems(memDesc)) RefExprERROR(s"Dead memory read not replaced during $name") 
          else rootMS
        case _  => super.transform(ref)
      }
    }

    DeadCheckTransformer.transform(consistent)
  }
}
