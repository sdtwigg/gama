package gama
package intermediate
package passes

object TightenExpr extends GamaPass {
  val name = "TightenExpr"
  // TODO: Assumes ExplodeConnect, ProcessReset has already been run
  //       Also, assuming RefM/VSelect already run (although, less critically)
  //       Assumes tree fully typed
  def transform(target: ElaboratedModule): ElaboratedModule = {
    def tighten(expr: ExprHW, targetT: TypeHW): ExprHW = 
      {for((targetSign, Some(targetWidth)) <- asPrimitiveTypeHW(targetT).flatMap(getRawBitsInfo(_)))
        yield tightenToTarget(expr, targetSign, targetWidth, passNote).getOrElse(
          RefExprERROR(s"Internal Error: Could not match $expr to $targetT (type checker should have interceded)")
        )
      }.getOrElse(expr)

    object Transformer extends ExprOnlyTransformTree {
      override def transform(expr: ExprHW) = expr match {
        /* // For now, none of these should need explicit extension
        case ExprUnary(op, target, rType, note) if false => {
          val newtarget = tighten(transform(target), rType)
          ExprUnary(op, newtarget, rType, note)
        }
        */
        case ExprBinary(op, left, right, rType, note)
          if Seq(OpAnd, OpOr, OpXor).contains(op) =>
        {
          val newleft  = tighten(transform(left), rType)
          val newright = tighten(transform(right), rType)
          ExprBinary(op, newleft, newright, rType, note)
        }
        // Note: Currently, do not bother extending comparison operations to have equal length
        // TODO: CONSIDER: is this ok?

        case ExprMux(cond, tc, fc, rType, note) => {
          val newtc = tighten(transform(tc), rType)
          val newfc = tighten(transform(fc), rType)
          ExprMux(cond, newtc, newfc, rType, note)
        }

        case _ => super.transform(expr)
      }

      override def transform(cmd: CmdHW) = cmd match {
        // Note: Assuming reg no longer has a reset
        case ConstDecl(symbol, expr, note) => {
          val newexpr = tighten(transform(expr), symbol.rType)
          ConstDecl(symbol, newexpr, note)
        }
        case ConnectStmt(sink, source, details, note) => {
          val newsource = tighten(transform(source), sink.rType)
          ConnectStmt(sink, newsource, details, note)
        }
        case MemWrite(desc, address, source, mask, note) =>
          (for((_, Some(srcwidth)) <- asPrimitiveTypeHW(source.rType).flatMap(getRawBitsInfo(_))) yield {
            val depthBits = math.max(BigInt(desc.depth).bitLength-1, 1) // TODO: bother with max?
            val newaddress = tighten(transform(address), PrimitiveNode( UBits(Some(depthBits)) ))
            val newsource = tighten(transform(source), desc.mType)
            val newmask = mask.map(mask => tighten(transform(mask), PrimitiveNode( UBits(Some(srcwidth)) )))
            MemWrite(desc, newaddress, newsource, newmask, note)
          }).getOrElse(CmdERROR(s"Internal Error: Unknown width for memory in $cmd", passNote))
        
        case MemRead(symbol, desc, address, en, note) => {
            val depthBits = math.max(BigInt(desc.depth).bitLength-1, 1) // TODO: bother with max?
            val newaddress = tighten(transform(address), PrimitiveNode( UBits(Some(depthBits)) ))
            MemRead(symbol, desc, newaddress, en, note)
          }
        case _ => super.transform(cmd) 
      }
    }

    Transformer.transform(target)
  }
}

