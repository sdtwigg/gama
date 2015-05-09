package gama
package intermediate
package passes

import scala.collection.mutable.{HashMap=>HMap, HashSet=>HSet, ListBuffer=>ListB}

object SymbolDeclAggregateExplode extends GamaPass {
  // WILL NOT ADJUST ANY RefIO OR RefMSelect, this must be done separately!
  // TODO: Assumes ExpandVSelect, AggregateConnectExplode already run
  // TODO: Uses DistributeRef
  def transform(target: ElaboratedModule): ElaboratedModule = {
    // Need these in order to properly split up aggregate Memories
    val symbolGen = new AppendableRefSymbolTable(target)
    val memDescGen = new AppendableRefSymbolTable(target)

    val path2sym_refTable = HMap.empty[PathTrace, RefSymbol]
    val sym2path_refTable = HMap.empty[RefSymbol, PathTrace]
      // PathTrace required so notes ignored upon lookup in the table
      // First one is used for actual replacement pass
      // Second one is for building a full replacement list
    val deadReferences = HSet.empty[RefSymbol]
      // If any of these are seen and thus not replaced, an error happened
    
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
          deadReferences += target.symbol // old target being exploded to invalidate its symbol
          fields.toSeq.sortBy(_._1).flatMap({case (field, eType) => {
            // First, build the new command
            val exprbuilder: ExprHW=>RefHW = expr => RefTLookup(expr, field, GamaNote())
            val newsymident = target.symbol.identifier.getOrElse("") + "$" + field
            val newcmdbuilder: Int=>CreatesRefSymbol = sid => {
              val newsym = RefSymbol(sid, Some(newsymident), eType, target.symbol.note)
              cmdbuilder(newsym, exprbuilder)
            }  
            val newcmd = symbolGen.grantNewSymbol(newcmdbuilder)
            // Now, build reference replacement table entries for it
            val oldPath: PathTrace = sym2path_refTable.getOrElse(target.symbol, PTStart(target.symbol))
            val newPath = PTField(oldPath, field)
            val newSym = newcmd.symbol
            path2sym_refTable += newPath -> newSym
            sym2path_refTable += newSym -> newPath
            // Finally, act recursively
            explodeCreatesRefSymbol(newcmd)
          }})
        }
        case VecHW(depth, eType) => {
          deadReferences += target.symbol // old target being exploded to invalidate its symbol
          (0 until depth).flatMap(idx => {
            // First, build the new command
            val exprbuilder: ExprHW=>RefHW = expr => RefVIndex(expr, idx, GamaNote())
            val newsymident = target.symbol.identifier.getOrElse("") + "$" + idx
            val newcmdbuilder: Int=>CreatesRefSymbol = sid => {
              val newsym = RefSymbol(sid, Some(newsymident), eType, target.symbol.note)
              cmdbuilder(newsym, exprbuilder )
            }  
            val newcmd = symbolGen.grantNewSymbol(newcmdbuilder)
            // Now, build reference replacement table entries for it
            val oldPath: PathTrace = sym2path_refTable.getOrElse(target.symbol, PTStart(target.symbol))
            val newPath = PTSelectOne(oldPath, idx)
            val newSym = newcmd.symbol
            path2sym_refTable += newPath -> newSym
            sym2path_refTable += newSym -> newPath
            // Finally, act recursively
            explodeCreatesRefSymbol(newcmd)
          })
        }
        case TypeHWUNKNOWN =>
          Some( CmdERROR("Unknown Type encountered during SymbolDeclAggregateExplode", target.note ) )
      }
    object DeclExplodeTransformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW): Iterable[CmdHW] = cmd match {
        case creator: CreatesRefSymbol => (explodeCreatesRefSymbol(creator))
        //case MemDecl(desc, note)
        case _ => super.multiTransform(cmd)
      }
    }
    val explodedDecl = target.copy(body = DeclExplodeTransformer.transform(target.body))

    // STEP 2: Ensure all aggregate sub-references distributed up as much as possible
    val smashedTarget = DistributeRef.transform(explodedDecl)

    // STEP 3: re
    object RefReplaceTransformer extends ExprTransformTreeFullSegregation {
      def makePath(expr: ExprHW): PathTrace = expr match {
        case RefVIndex(source, index, _)  => PTSelectOne(makePath(source), index)
        case RefTLookup(source, field, _) => PTField(makePath(source), field)

        case RefVSelect(source, _, _) => PTSelectALL(makePath(source)) // This shouldn't occur....
        case _ => PTStart(expr)
      }
      override def transform(ref: RefHW): RefHW  = ref match {
        case RefVIndex(_,_,_) | RefTLookup(_,_,_) => path2sym_refTable.get(makePath(ref)).getOrElse(ref)
        case _  => super.transform(ref)
      }
    }
    val consistent = smashedTarget.copy(body = RefReplaceTransformer.transform(smashedTarget.body))

    // STEP 4: Make sure no references to dead symbols persist in the tree
    object DeadCheckTransformer extends ExprTransformTreeFullSegregation {
      override def transform(ref: RefHW): RefHW  = ref match {
        case symbol @ RefSymbol(_,_,_,_) =>
          if(deadReferences(symbol)) RefExprERROR("Dead Reference not replaced during DeclAggregateExplode") 
          else symbol
        case _  => super.transform(ref)
      }
    }

    consistent.copy(body = DeadCheckTransformer.transform(consistent.body))
  }
}
