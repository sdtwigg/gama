package gama
package internal
package frontend

class ExprScanTree {
  def scan(cmd: CmdHW): Unit = cmd match {
    case WireDecl(symbol)  => scan(symbol) 
    case RegDecl(symbol)   => scan(symbol) 
    case ConstDecl(symbol, expr) => {scan(symbol); scan(expr)}
    case AliasDecl(symbol, ref) => {scan(symbol); scan(ref)}
    case BlockHW(stmts) => stmts.foreach(scan(_))
    case WhenHW(cond, tc, fc) => {scan(cond); scan(tc); scan(fc)}

    case ConnectStmt(sink, source, _)  => {scan(sink); scan(source)}
    case BiConnectStmt(left, right, _) => {scan(left); scan(right)}

    case MemDecl(_) | SubModuleDecl(_,_) =>
  }

  def scan(expr: ExprHW): Unit = expr match {
    case ExprUnary(_, target, _) => {scan(target)}
    case ExprBinary(_, left, right, _) => {scan(left); scan(right)}
    case ExprMux(cond, tc, fc, rType) => {scan(cond); scan(tc); scan(fc)}

    case RefMSelect(_, selector) => scan(selector)
    case RefVIndex(source, _) => scan(source)
    case RefVSelect(source, selector) => {scan(source); scan(selector)}
    case RefTLookup(source, _) => scan(source)
    case RefExtract(source, _, _, _) => {scan(source)}

    case ExprLit(_, _) | RefSymbol(_, _, _) | RefIO(_) | RefExprERROR(_) => 
  }
}

trait LinkedTypeScanTree {
  // Should this always use TyperWidthInfer.bypassCompRef 
  def startscan(leader: ExprHW, followers: Iterable[ExprHW]): Unit = {
    val newf = followers.map(f => (f.rType, TTStart(f)))
    pathscan(leader.rType, TTStart(leader), newf)
  }
  def pathscan(leader: TypeHW, leadPath: TypeTrace,
               followers: Iterable[Tuple2[TypeHW, TypeTrace]]): Unit = 
    leader match {
      case TupleHW(fields) =>
        for((field, lEType) <- fields) {
          val newfollowers = for {
            (fType, fPath) <- followers // fType is TypeHW
            fTType <- asTupleHW(fType)  // fTType as TupleHW
            fEType <- fTType.fields find(_._1 == field) map(_._2)
          } yield (fEType, TTField(fPath, field)): Tuple2[TypeHW, TypeTrace]
          pathscan(lEType, TTField(leadPath, field), newfollowers)
        }
      case VecHW(_, lEType) => {
        val newfollowers = for {
          (fType, fPath) <- followers // fType as TypeHW
          fVType <- asVecHW(fType)    // fVType as VecHW
          fEType = fVType.elemType
        } yield (fEType, TTIndexALL(fPath)): Tuple2[TypeHW, TypeTrace]
        pathscan(lEType, TTIndexALL(leadPath), newfollowers)
      }
      case lPType: PrimitiveTypeHW => {
        val newfollowers = for {
          (fType, fPath) <- followers        // fType as TypeHW
          fPType <- asPrimitiveTypeHW(fType) // fPType as PrimitiveTypeHW
        } yield (fPType, fPath): Tuple2[PrimitiveTypeHW, TypeTrace]
        leafwork(lPType, leadPath, newfollowers)
      }

      case TypeHWUNKNOWN => // TODO: Error? Also, what if a follower is lost?
    }

  def leafwork(leader: PrimitiveTypeHW, leadPath: TypeTrace,
               followers: Iterable[Tuple2[PrimitiveTypeHW, TypeTrace]]): Unit
}

