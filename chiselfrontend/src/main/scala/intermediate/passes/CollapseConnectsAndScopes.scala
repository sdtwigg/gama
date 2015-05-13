package gama
package intermediate
package passes

import scala.collection.mutable.{HashMap=>HMap, HashSet=>HSet, ListBuffer=>ListB}

object CollapseConnectsAndScopes extends GamaPass {
  val name = "CollapseConnectsAndScopes"

  sealed trait CollapseTarget
  case class CTSymbol(symbol: RefSymbol)
  case class CTIO(path: IOPathTrace)
  case class CTMem(mem: MemDesc)

  sealed trait ListedCmdHW
  case class UnresolvedCmdHW(cmd: CmdHW)
  case class ResolvedCmdHW(cmd: CmdHW)

  def transform(target: ElaboratedModule): ElaboratedModule = {
    def collapseBlock(cmds: Iterable[CmdHW], parentScope: ScopeContainer): CmdHW = ???
    target
  }

  class ScopeContainer(parent: Option[ScopeContainer]) {
    private val resolvedDecl = ListB.empty[CmdHW] // Emit first since connect, other cmds may use
    private val otherCmds = ListB.empty[ListedCmdHW]    // Emit second (nested scopes go here)
    private val resolvedConnect = ListB.empty[CmdHW]  // Emit last since use decl and maybe other cmds
  }

  class ConnectJournal
}
