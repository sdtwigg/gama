package gama
package intermediate

class ExprScanTree {
  def scan(cmd: CmdHW): Unit = cmd match {
    case WireDecl(symbol, _)  => scan(symbol) 
    case RegDecl(symbol, reset, _)   => {scan(symbol); reset.foreach({
      case (ren, rval) => {scan(ren); scan(rval)}
    })}
    case ConstDecl(symbol, expr, _) => {scan(symbol); scan(expr)}
    case AliasDecl(symbol, ref, _) => {scan(symbol); scan(ref)}
    case BlockHW(stmts, _) => stmts.foreach(scan(_))
    case WhenHW(cond, tc, fc, _) => {scan(cond); scan(tc); scan(fc)}

    case ConnectStmt(sink, source, _,_)  => {scan(sink); scan(source)}
    case BiConnectStmt(left, right, _,_) => {scan(left); scan(right)}

    case MemDecl(_,_) | SubModuleDecl(_,_,_) =>
  }

  def scan(expr: ExprHW): Unit = expr match {
    case ExprUnary(_, target, _,_) => {scan(target)}
    case ExprBinary(_, left, right, _,_) => {scan(left); scan(right)}
    case ExprMux(cond, tc, fc, rType, _) => {scan(cond); scan(tc); scan(fc)}

    case RefMSelect(_, selector, _) => scan(selector)
    case RefVIndex(source, _,_) => scan(source)
    case RefVSelect(source, selector, _) => {scan(source); scan(selector)}
    case RefTLookup(source, _,_) => scan(source)
    case RefExtract(source, _,_,_,_) => {scan(source)}

    case ExprLit(_,_,_) | RefSymbol(_,_,_,_) | RefIO(_,_) | RefExprERROR(_) => 
  }
}

trait LinkedTypeScanTree {
  def start(leader: ExprHW, followers: Iterable[ExprHW]): Unit = {
    val (ltype, lpath) = TyperWidthInferer.bypassCompRef(leader)
    val newf = followers.map(TyperWidthInferer.bypassCompRef(_))
    pathscan(ltype, lpath, newf)
  }
  def startguided(details: ConnectDetails, leader: ExprHW, followers: Iterable[ExprHW]): Unit = {
    val (ltype, lpath) = TyperWidthInferer.bypassCompRef(leader)
    val newf = followers.map(TyperWidthInferer.bypassCompRef(_))
    guidedscan(details, ltype, lpath, newf)
  }
  def startbiguided(details: BiConnectDetails, left: ExprHW, right: ExprHW): Unit = {
    val (ltype, lpath) = TyperWidthInferer.bypassCompRef(left)
    val (rtype, rpath) = TyperWidthInferer.bypassCompRef(right)
    biguidedscan(details, ltype, lpath, rtype, rpath)
  }
  def pathscan(leader: TypeHW, leadPath: TypeTrace,
               followers: Iterable[Tuple2[TypeHW, TypeTrace]]): Unit = 
    leader match {
      case TupleHW(fields) =>
        for((field, lEType) <- fields) {
          val newfollowers = for {
            (fType, fPath) <- followers // fType is TypeHW
            fTType <- asTupleHW(fType)  // fTType as TupleHW
            fEType <- fTType.fields.get(field)
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
  def guidedscan(details: ConnectDetails, leader: TypeHW, leadPath: TypeTrace,
                 followers: Iterable[Tuple2[TypeHW, TypeTrace]]): Unit = 
    details match {
      case ConnectAll => pathscan(leader, leadPath, followers)

      case ConnectVec(_,elemd) => for{
        lType <- asVecHW(leader)
        lEType = lType.elemType
        lPath  = TTIndexALL(leadPath)
      } {
        val newfollowers = for {
          (fType, fPath) <- followers
          fVType <- asVecHW(fType)
          fEType = fVType.elemType
        } yield (fEType, TTIndexALL(fPath)): Tuple2[TypeHW, TypeTrace]
        guidedscan(elemd, lEType, lPath, newfollowers)
      }

      case ConnectTuple(fieldds) => for {
        (field, elemd) <- fieldds  // String, ConnectDetails
        lType <- asTupleHW(leader) // lType as TupleHW
        lEType <- lType.fields.get(field)
        lPath = TTField(leadPath, field)
      } {
        val newfollowers = for {
          (fType, fPath) <- followers // fType is TypeHW
          fTType <- asTupleHW(fType)  // fTType as TupleHW
          fEType <- fTType.fields.get(field)
        } yield (fEType, TTField(fPath, field)): Tuple2[TypeHW, TypeTrace]
        guidedscan(elemd, lEType, lPath, newfollowers)
      }
    }
  def biguidedscan(details: BiConnectDetails,
                 leftType: TypeHW, leftPath: TypeTrace,
                 rightType: TypeHW, rightPath: TypeTrace): Unit = 
    details match {
      case BiConnectToLeft  => pathscan(leftType,  leftPath,  Some((rightType, rightPath)))
      case BiConnectToRight => pathscan(rightType, rightPath, Some((leftType,  leftPath)))

      case BiConnectVec(_, elemd) => for{
        lType <- asVecHW(leftType)
        lEType = lType.elemType
        lPath  = TTIndexALL(leftPath)
        rType <- asVecHW(rightType)
        rEType = rType.elemType
        rPath  = TTIndexALL(rightPath)
      } { biguidedscan(elemd, lEType, lPath, rEType, rPath) }

      case BiConnectTuple(fieldds) => for {
        (field, elemd) <- fieldds  // String, ConnectDetails
        lType <- asTupleHW(leftType) // lType as TupleHW
        lEType <- lType.fields.get(field)
        lPath = TTField(leftPath, field)
        rType <- asTupleHW(rightType) // lType as TupleHW
        rEType <- rType.fields.get(field)
        rPath = TTField(rightPath, field)
      } { biguidedscan(elemd, lEType, lPath, rEType, rPath) }
    }

  def leafwork(leader: PrimitiveTypeHW, leadPath: TypeTrace,
               followers: Iterable[Tuple2[PrimitiveTypeHW, TypeTrace]]): Unit
}

