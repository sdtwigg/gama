package gama
package intermediate
package passes

object ProcessReset extends GamaPass {
  // Turn all RegDecl with some reset logic into those without but with a reset connection
  //   at the end of the declared scope for the reset (thus, at this point guaranteed to be
  //   the last reset)
  def transform(target: ElaboratedModule): ElaboratedModule = {
    object Transformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW) = cmd match {
        case BlockHW(cmds, note) => {
          // First, for RegDecl with resets that are in a BlockHW,
          //   want the reset added to the end of the declaring scope
          val (newcmds: List[CmdHW], append: List[Option[CmdHW]]) = cmds.map(_ match {
            case RegDecl(symbol, Some((ren, rval)), note) =>
              ( RegDecl(symbol, None, note),
                Some( WhenHW(ren, ConnectStmt(symbol, rval, ConnectAll, note), NOPHW, GamaNote()) ) )
            case cmd => (super.transform(cmd), None)
          }).unzip
          Some( BlockHW((newcmds ++ append.flatten), note) )
        }
        case RegDecl(symbol, Some((ren, rval)), note) => Seq(
          RegDecl(symbol, None, note),
          WhenHW(ren, ConnectStmt(symbol, rval, ConnectAll, note), NOPHW, GamaNote())
        ) // Second, catch RegDecl that are not enclosed by a BlockHW (e.g. in a strangely written WhenHW)
          // Since they are alone (and thus are never assigned to), can just emit the reset logic immediately
        case _ => super.multiTransform(cmd)
      }
    }

    target.copy(body = Transformer.transform(target.body))
  }
}

