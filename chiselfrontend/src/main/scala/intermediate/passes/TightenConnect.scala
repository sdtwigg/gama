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
            (sinkSign, Some(sinkWidth)) <- asPrimitiveTypeHW(sink.rType).flatMap(getRawBitsInfo(_))
          } yield {
            val newSource = tightenToTarget(source, sinkSign, sinkWidth, passNote).getOrElse(
              RefExprERROR(s"Internal Error: Could not match $source to $sink (type checker should have interceded)")
            )
            ConnectStmt(sink, newSource, details, note)
          }).orElse(Some(cmd))

        case _ => super.multiTransform(cmd)
      }
    }
    Transformer.transform(target)
  }
}

