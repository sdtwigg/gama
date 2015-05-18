package gama
package intermediate
package passes

import scala.collection.mutable.{HashMap=>HMap, LinkedHashMap=>LHMap, HashSet=>HSet, ListBuffer=>ListB}

object CollapseConnectsAndScopes extends GamaPass {
  val name = "CollapseConnectsAndScopes"

  sealed trait CollapseTarget
  case class CTSymbol(symbol: RefSymbol) extends CollapseTarget
  case class CTIO(module: ModuleRef, field: String) extends CollapseTarget
    // Don't bother with IOPathTrace since this pass assumes flat IO
  case class CTMem(mem: MemDesc) extends CollapseTarget

  sealed trait OpenCondition
  case object NoCond extends OpenCondition
  case class OpenWhen(cond: ExprHW, parent: OpenCondition) extends OpenCondition
  case class OpenElse(owhen: OpenWhen) extends OpenCondition
  // Instead of updating the condition on all open journals, just sync accessed journal as new condition encountered
  //   by closing old conditions to ancestor (and then opening up new ones until the current condition opened)

  def transform(target: ElaboratedModule): ElaboratedModule = {
    // TODO: Assumes InternalAggregateExplode, CircuitFlattenIO run (thus very minimal presence of aggregates)
    //       Assumes ExpandM/VSelect run (no RefVSelect or RefMSelect exist in the tree)
    val masterScope = new ScopeContainer(None, NoCond)
    masterScope.injectTopIO(target.io)
    val newbody = BlockHW(masterScope.process(Some(target.body)).toList, passNote)
    target.copy(body = newbody)
  }

  final class ScopeContainer(parent: Option[ScopeContainer], val condStack: OpenCondition) {
    private[this] val scopedJournals = LHMap.empty[CollapseTarget, ConnectJournal]
    def injectTopIO(io: TypeHW): Unit = io match {
      case TupleHW(fields) => for( (field, elemType) <- fields.toSeq.sortBy(_._1)) {
        // Create WireJournals for each input
        elemType match {
          case PrimitivePort(_, DirectionIO.Output) => {
            val ct = CTIO(ModuleThis(io), field)
            scopedJournals += ct -> WireJournal(ct, this)
          }
          case PrimitivePort(_, DirectionIO.Input) => {} // Do nothing
          case _ => throw new Exception(s"During $name, Module found with non-flattened ioType")
        }
      }
      case _ => throw new Exception(s"During $name, Module found with non-TupleHW ioType")
    }

    protected def lookup(key: CollapseTarget): Option[ConnectJournal] =
      scopedJournals.get(key) orElse parent.flatMap(_.lookup(key)) 
    protected def constructCT(ref: ExprHW): Option[CollapseTarget] = ref match {
      case symbol @ RefSymbol(_,_,_,_) => Some( CTSymbol(symbol) )
      case RefTLookup(RefIO(mod,_),field,_) => Some( CTIO(mod, field) )
      case RefExtract(source,_,_,_) => constructCT(source)
      case _ => None
    }

    private[this] def journalProcess(target: CollapseTarget, cmd: CmdHW): Iterable[CmdHW] =
      lookup(target) match {
        case Some(journal) => journal.process(cmd, this)
        case None => Some(CmdERROR(s"Unresolved reference in $name: $target", cmd.note)) 
      }
    def process(cmds: Iterable[CmdHW]): Iterable[CmdHW] = {
      val resolvedCmds = cmds.flatMap(cmd => cmd match {
        case WireDecl(symbol, _) => {
          val ct = CTSymbol(symbol)
          scopedJournals += ct -> WireJournal(ct, this)
          Some(cmd)
        }
        case RegDecl(symbol, None, _) => { 
          val ct = CTSymbol(symbol)
          scopedJournals += ct -> RegJournal(ct, this)
          Some(cmd)
        }
        case SubModuleDecl(details,_,_) => {
          details.ioType match {
            case TupleHW(fields) => for( (field, elemType) <- fields.toSeq.sortBy(_._1)) {
              // Create WireJournals for each input
              elemType match {
                case PrimitivePort(_, DirectionIO.Input) => {
                  val ct = CTIO(details, field)
                  scopedJournals += ct -> WireJournal(ct, this)
                }
                case PrimitivePort(_, DirectionIO.Output) => {} // Do nothing
                case _ => throw new Exception(s"During $name, SubModuleDecl found with non-flattened ioType")
              }
            }
            case _ => throw new Exception(s"During $name, SubModuleDecl found with non-TupleHW ioType")
          }
          Some(cmd)
        }

        case ConnectStmt(sink,_,_,_) => constructCT(sink) match {
          case Some(ct) => journalProcess(ct, cmd) 
          case None => Some(CmdERROR(s"Invalid sink encountered during $name: $sink", cmd.note))
        }
        
        case MemDecl(memdesc, _) => { // Create MemJournal
          val ct = CTMem(memdesc)
          scopedJournals += ct -> MemJournal(ct, this)
          Some(cmd)
        }
        case MemWrite(desc,_,_,_,_) => journalProcess(CTMem(desc), cmd)
        case MemRead(_,desc,_,_,_) => journalProcess(CTMem(desc), cmd)

        case BlockHW(statements, _) => {
          new ScopeContainer(Some(this), condStack).process(statements)
        }
        case WhenHW(cond, tc, fc, _) => {
          val condtc = OpenWhen(cond, condStack)
          val tcScope = new ScopeContainer(Some(this), condtc)
          val condfc = OpenElse(condtc)
          val fcScope = new ScopeContainer(Some(this), condfc)
          tcScope.process(Some(tc)) ++ tcScope.process(Some(fc))
        }
        
        case ConstDecl(_,_,_) | CmdERROR(_,_) => Some(cmd)

        case AliasDecl(_,_,_) | BiConnectStmt(_,_,_,_) | RegDecl(_, Some(_), _) =>
          Some( CmdERROR(s"Disallowed command $cmd encountered during $name", cmd.note) )
      })
      val resolvedJournals = scopedJournals.values.flatMap(_.resolve) 
      resolvedCmds ++ resolvedJournals
    }
  }

  sealed abstract class ConnectJournal(scope: ScopeContainer) {
    def process(cmd: CmdHW, scope: ScopeContainer): Iterable[CmdHW]
    def resolve: Iterable[CmdHW]
  }
  case class WireJournal(target: CollapseTarget, scope: ScopeContainer) extends ConnectJournal(scope) {
    // translate when into muxes, needs defaults
    def process(cmd: CmdHW, scope: ScopeContainer): Iterable[CmdHW] = None // TODO
    def resolve: Iterable[CmdHW] = None // TODO
  }
  case class RegJournal(target: CTSymbol, scope: ScopeContainer) extends ConnectJournal(scope) {
    // track the whens and just emit that when tree
    def process(cmd: CmdHW, scope: ScopeContainer): Iterable[CmdHW] = None // TODO
    def resolve: Iterable[CmdHW] = None // TODO
  }
  case class MemJournal(target: CTMem, scope: ScopeContainer) extends ConnectJournal(scope) {
    // just tracks scope, when memread/memwrite encountered, immediately emit to scope
    def process(cmd: CmdHW, scope: ScopeContainer): Iterable[CmdHW] = None // TODO
    def resolve: Iterable[CmdHW] = None
  }
}
