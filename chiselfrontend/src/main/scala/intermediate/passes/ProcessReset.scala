package gama
package intermediate
package passes

object ProcessReset extends GamaPass {
  val name = "ProcessReset"
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
            case RegDecl(symbol, clock, Some((ren, rval)), note) =>
              ( RegDecl(symbol, clock, None, note),
                Some( WhenHW(ren, ConnectStmt(symbol, rval, ConnectAll, note), NOPHW, passNote) ) )
            case cmd => (super.transform(cmd), None)
          }).unzip
          Some( BlockHW((newcmds ++ append.flatten), note) )
        }
        case RegDecl(symbol, clock, Some((ren, rval)), note) => Seq(
          RegDecl(symbol, clock, None, note),
          WhenHW(ren, ConnectStmt(symbol, rval, ConnectAll, note), NOPHW, passNote)
        ) // Second, catch RegDecl that are not enclosed by a BlockHW (e.g. in a strangely written WhenHW)
          // Since they are alone (and thus are never assigned to), can just emit the reset logic immediately
        case _ => super.multiTransform(cmd)
      }
    }

    Transformer.transform(target)
  }
}

