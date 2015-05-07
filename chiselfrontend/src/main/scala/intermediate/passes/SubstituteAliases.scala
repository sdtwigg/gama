package gama
package intermediate
package passes

object SubstituteAliases extends GamaPass {
  def transform(target: ElaboratedModule): ElaboratedModule = {
    val dealiaser = new Dealiaser

    val aliasfinder = new ExprScanTree {
      override def scan(cmd: CmdHW): Unit = {
        cmd match {
          case AliasDecl(symbol, ref, _) => dealiaser.addAlias(symbol, ref)
          case _ =>
        }
        super.scan(cmd)
      }
    }
    aliasfinder.scan(target.body)

    object Transformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW) = cmd match {
        case AliasDecl(symbol, ref, note) => Some( ConstDecl(symbol, ref, note) )
        case ConnectStmt(sink, source, details, note) =>
          Some( ConnectStmt(dealiaser.dealias(sink), source, details, note) )
        case BiConnectStmt(left, right, details, note) => 
          Some( BiConnectStmt(dealiaser.dealias(left), dealiaser.dealias(right), details, note) )
        case _ => super.multiTransform(cmd)
      }
    }
    ElaboratedModule(target.io, Transformer.transform(target.body))
  }
}
