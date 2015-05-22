package gama
package intermediate

object LocalExprTyper extends GamaPass {
  val name = "LocalExprTyper"
  object Transformer extends ExprOnlyTransformTree {
    // Helper classes
    case class NSMaker(buildNS: ((Boolean,Boolean), Option[Int]) => Option[NodeStore]) {
      // The booleans are whether the left and right types are signed, Option[Int] is new width
      def apply(leftNS: NodeStore, rightNS: NodeStore, func: (Int,Int)=>Int): Option[NodeStore] = {
        // func from known left and right width to new known width
        for{
          (leftSign,  leftWopt)  <- getRawBitsInfo(leftNS)
          (rightSign, rightWopt) <- getRawBitsInfo(rightNS)
          newW = (for(leftW <- leftWopt; rightW <- rightWopt) yield func(leftW, rightW)): Option[Int]
          newStorage <- buildNS((leftSign, rightSign), newW)
        } yield newStorage
      }
    }
    val preferSBits = NSMaker((signs, newW) => (signs._1, signs._2) match {
      case (true, _) | (_, true) => Some( SBits(newW) )
      case (false, false)        => Some( UBits(newW) )
    })
    val requireSame = NSMaker((signs, newW) => (signs._1, signs._2) match {
      case (true, true)   => Some( SBits(newW) )
      case (false, false) => Some( UBits(newW) )
      case _ => None
    })
    val matchLeft = NSMaker((signs, newW) => signs._1 match {
      case true  => Some( SBits(newW) )
      case false => Some( UBits(newW) )
    })
    val forceUBits = NSMaker((_, newW) => Some( UBits(newW)) )

    // The actual typer
    override def transform(expr: ExprHW): ExprHW = expr match {
      case ExprUnary(op, target, rType, note) if rType == TypeHWUNKNOWN => {
        val newTarget = transform(target)
        val newType = (for{
          targetT <- asPrimitiveTypeHW(newTarget.rType)
          newStorage <- (op match {
            case OpIDENT  => Some( targetT.storage )
            case OpAsUInt => for(width <- getWidth(targetT)) yield UBits(width)
            case OpAsSInt => for(width <- getWidth(targetT)) yield SBits(width)
            case OpNot    => Some( targetT.storage )
            case OpXorRed => Some( UBits(Some(1)) )
          }): Option[NodeStore]
        } yield PrimitiveNode(newStorage)).getOrElse(TypeHWUNKNOWN)
        ExprUnary(op, newTarget, newType, note)
      }

      case ExprBinary(op, left, right, rType, note) if rType == TypeHWUNKNOWN => {

        // Actual logic
        val newLeft = transform(left)
        val newRight = transform(right)
        val newType = (for{
          leftT  <- asPrimitiveTypeHW(newLeft.rType)
          rightT <- asPrimitiveTypeHW(newRight.rType)
          leftNS = leftT.storage; rightNS = rightT.storage
          newStorage <- (op match {
            case OpPlus | OpSubt      => preferSBits(leftNS, rightNS, (l,r) => math.max(l,r))
            case OpMult               => preferSBits(leftNS, rightNS, (l,r) => l+r)
            case OpDiv                => preferSBits(leftNS, rightNS, (l,r) => l)
            case OpMod                => preferSBits(leftNS, rightNS, (l,r) => r)
            case OpAnd | OpOr | OpXor => requireSame(leftNS, rightNS, (l,r) => math.max(l,r))
            case OpPadTo              =>   matchLeft(leftNS, rightNS, (l,r) => math.max(l,r))
            case OpCat                =>   matchLeft(leftNS, rightNS, (l,r) => l+r)
            case OpLShft              =>   matchLeft(leftNS, rightNS, (l,r) => l+(1 << r)-1)
            case OpRShft              =>   matchLeft(leftNS, rightNS, (l,r) => l)
            case OpEqual | OpNotEq | OpLess | OpLeEq | OpGrt | OpGrEq => Some( UBits(Some(1)) )
          }): Option[NodeStore]
        } yield PrimitiveNode(newStorage)).getOrElse(TypeHWUNKNOWN)

        ExprBinary(op, newLeft, newRight, newType, note)
      }

      case ExprMux(cond, tc, fc, rType, note) if rType == TypeHWUNKNOWN => {
        val newCond = transform(cond)
        val newTC = transform(tc)
        val newFC = transform(fc)
        def muxRetVal(leftT: TypeHW, rightT: TypeHW): TypeHW = (leftT, rightT) match {
          case (VecHW(ld, leT), VecHW(rd, reT)) if ld == rd => VecHW(ld, muxRetVal(leT, reT))
          case (TupleHW(lfields), TupleHW(rfields)) => TupleHW(( for{
            (field, leT) <- lfields
            reT <- rfields.get(field)
          } yield (field, muxRetVal(leT, reT)) ).toMap)
          case (leT: PrimitiveTypeHW, reT: PrimitiveTypeHW) => (for{
            newStorage <- requireSame(leT.storage, reT.storage, (l,r) => math.max(l,r))
          } yield PrimitiveNode(newStorage)).getOrElse(TypeHWUNKNOWN)
          case _ => TypeHWUNKNOWN
        }
        val newType = muxRetVal(newTC.rType, newFC.rType)
        ExprMux(newCond, newFC, newTC, newType, note)
      }

      case ExprLit(litvalue, rType, note) if rType == TypeHWUNKNOWN => {
        def convertLitTree(in: LitTree): TypeHW = in match {
          case LitRawBits(_, width, signed) => PrimitiveNode(signed match {
            case true  => SBits(Some(width))
            case false => UBits(Some(width))
          })
          case LitVec(elements) => VecHW(elements.length,
            elements.headOption.map(convertLitTree(_)).getOrElse(TypeHWUNKNOWN)
          )
          case LitTuple(fields) => TupleHW(
            fields.map({case (field, eLT) => (field, convertLitTree(eLT))})
          )
        }
        ExprLit(litvalue, convertLitTree(litvalue), note)
      }

      // Note, no Ref* because either already computed or not determinable locally
      case _ => super.transform(expr) // type known so just go deeper
    }
    // TODO: Also handle Cmds?
  }

  def transform(target: ElaboratedModule): ElaboratedModule = Transformer.transform(target)
  def transform(target: ExprHW): ExprHW = Transformer.transform(target)
}
