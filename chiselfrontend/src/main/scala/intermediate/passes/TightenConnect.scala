package gama
package intermediate
package passes

object TightenConnect extends GamaPass {
  val name = "TightenConnect"
  // TODO: Assumes ExplodeConnect has already been run
  //       Assumes tree fully typed
  def transform(target: ElaboratedModule): ElaboratedModule = {
    object Transformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW) = cmd match {
        case ConnectStmt(sink, source, details, note) => (for{
            sinkAsPrimitive <- asPrimitiveTypeHW(sink.rType)
            (sinkSign: Boolean, sinkWidthO) <- getRawBitsInfo(sinkAsPrimitive)
            sinkWidth: Int <- sinkWidthO // TODO: handle this being None better

            sourceAsPrimitive <- asPrimitiveTypeHW(source.rType)
            (sourceSign: Boolean, sourceWidthO) <- getRawBitsInfo(sourceAsPrimitive)
            sourceWidth: Int <- sourceWidthO // TODO: handle this being None better
          } yield {
            // TODO: Can probably extract this to a helper function....
            val newSource =
              if(sinkWidth == sourceWidth) {
                if(sinkSign != sourceSign)
                  if(sinkSign)
                    ExprUnary(OpAsSInt, source, PrimitiveNode(SBits(Some(sourceWidth))), passNote)
                  else
                    ExprUnary(OpAsUInt, source, PrimitiveNode(UBits(Some(sourceWidth))), passNote)
                else source
              }
              else if(sinkWidth < sourceWidth) {
                val extract = RefExtract(source, sinkWidth-1, 0, passNote)
                if(sinkSign)
                  ExprUnary(OpAsSInt, extract, PrimitiveNode(SBits(Some(sinkWidth))), passNote)
                else extract
              }
              else { // sinkWidth > sourceWidth
                val rType = PrimitiveNode(UBits(Some(sinkWidth)))
                val wdiff = sinkWidth - sourceWidth
                val allzeros = ExprLitU(0, wdiff)

                val extension = if(sourceSign) {
                  val allones = ExprLitU((BigInt(1) << wdiff)-1, wdiff)
                  assert(allones.rType == allzeros.rType, s"Internal Error: type mismatch for $allones vs $allzeros")
                  val sourceMSB = RefExtract(source, sourceWidth-1, sourceWidth-1, passNote)
                  ExprMux(sourceMSB, allones, allzeros, allones.rType, passNote)
                } else allzeros

                val padded = ExprBinary(OpCat, extension, source, rType, passNote)
                if(sinkSign)
                  ExprUnary(OpAsSInt, padded, PrimitiveNode(SBits(Some(sinkWidth))), passNote)
                else padded
              }
            ConnectStmt(sink, newSource, details, note)
          }).orElse(Some(cmd))

        case _ => super.multiTransform(cmd)
      }
    }
    Transformer.transform(target)
  }
}

