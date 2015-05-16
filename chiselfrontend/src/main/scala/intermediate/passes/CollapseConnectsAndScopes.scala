package gama
package intermediate
package passes

import scala.collection.mutable.{HashMap=>HMap, HashSet=>HSet, ListBuffer=>ListB}

object CollapseConnectsAndScopes extends GamaPass {
  val name = "CollapseConnectsAndScopes"

  sealed trait CollapseTarget
  case class CTSymbol(symbol: RefSymbol)
  case class CTIO(module: ModuleRef, field: String) // Don't bother with IOPathTrace since this pass assumes flat IO
  case class CTMem(mem: MemDesc)

  sealed trait OpenCondition
  case class OpenWhen(cond: ExprHW, parent: Option[OpenCondition]) extends OpenCondition
  case class OpenElse(owhen: OpenWhen) extends OpenCondition
  // Instead of updating the condition on all open journals, just sync accessed journal as new condition encountered
  //   by closing old conditions to ancestor (and then opening up new ones until the current condition opened)

  def transform(target: ElaboratedModule): ElaboratedModule = {
    // TODO: Assumes InternalAggregateExplode, CircuitFlattenIO run (thus very minimal presence of aggregates)
    //       Assumes ExpandM/VSelect run (no RefVSelect or RefMSelect exist in the tree)
    def collapseBlock(cmds: Iterable[CmdHW], parentScope: Option[ScopeContainer]): Iterable[CmdHW] = {
      val scope = new ScopeContainer(parentScope)
      ???
    }
    target
  }

  class ScopeContainer(parent: Option[ScopeContainer]) {
    private val resolvedCmds = ListB.empty[CmdHW]
    private val scopedJournals = HMap.empty[CollapseTarget, ConnectJournal]
  }

  sealed abstract class ConnectJournal(scope: ScopeContainer)
  final class WireJournal(target: CollapseTarget, scope: ScopeContainer) extends ConnectJournal(scope)
    // translate when into muxes, needs defaults
  final class RegJournal(target: CTSymbol, scope: ScopeContainer) extends ConnectJournal(scope)
    // track the whens and just emit that when tree
  final class MemJournal(mem: MemDesc, scope: ScopeContainer) extends ConnectJournal(scope)
    // just tracks scope, when memread/memwrite encountered, immediately emit to scope
}
