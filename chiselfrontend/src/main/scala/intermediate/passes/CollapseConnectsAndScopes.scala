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
          tcScope.process(Some(tc)) ++ fcScope.process(Some(fc))
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
    def process(cmd: CmdHW, cmdScope: ScopeContainer): Iterable[CmdHW]
    def resolve: Iterable[CmdHW]

    def calcDiff(checkCond: OpenCondition): OpenCondition =
      // Returns an OpenCondition that is the 'difference' between scope.condStack and checkCond
      // Somewhat assumes scope.condStack is an ancestor of checkCond (otherwise, checkCond is emitted)
      // TODO: Can have this return a 'reduced' OpenCondition?
      if(checkCond == scope.condStack) NoCond
      else checkCond match {
        case OpenWhen(cond, parent) => OpenWhen(cond, calcDiff(parent))
        case OpenElse(OpenWhen(cond, parent)) => OpenElse(OpenWhen(cond, calcDiff(parent)))
          // Note, invariants violated if condStack == that inner when so not checking for it
        case NoCond => NoCond
          // should never occur
      }
  }
  case class WireJournal(target: CollapseTarget, scope: ScopeContainer) extends ConnectJournal(scope) {
    // translate when into muxes, needs defaults
    def process(cmd: CmdHW, cmdScope: ScopeContainer): Iterable[CmdHW] = None // TODO
    def resolve: Iterable[CmdHW] = None // TODO
  }
  case class RegJournal(target: CTSymbol, scope: ScopeContainer) extends ConnectJournal(scope) {
    // track the whens and just emit that when tree
    // Should sort-of track MemWrite emission
    def process(cmd: CmdHW, cmdScope: ScopeContainer): Iterable[CmdHW] = None // TODO
    def resolve: Iterable[CmdHW] = None // TODO
  }
  case class MemJournal(target: CTMem, scope: ScopeContainer) extends ConnectJournal(scope) {
    // just tracks scope, when memread/memwrite encountered, immediately emit to scope
    def process(cmd: CmdHW, cmdScope: ScopeContainer): Iterable[CmdHW] = cmd match {
      case MemRead(symbol, desc, address, en, note) => {
        val fullEnStack = calcDiff(if(en != ExprLitU(1)) OpenWhen(en, cmdScope.condStack) else cmdScope.condStack)
          // Only pop en into the condition if it is non-trivial
        def notExpr(target: ExprHW): ExprHW = ExprUnary(OpNot, target, PrimitiveNode(UBits(Some(1))), passNote)
        def andExpr(left: ExprHW, right: ExprHW): ExprHW = ExprBinary(OpAnd, left, right, PrimitiveNode(UBits(Some(1))), passNote)
        def condToExpr(target: OpenCondition): ExprHW = target match {
          case NoCond => ExprLitU(1)
          case OpenWhen(cond, NoCond) => cond
          case OpenElse(OpenWhen(cond, NoCond)) => notExpr(cond)
          // These 2 with nested NoCond are to avoid extraneous ExprLitU(1) in the full condition

          case OpenWhen(cond, parent) => andExpr(cond, condToExpr(parent))
          case OpenElse(OpenWhen(cond, parent)) => andExpr(notExpr(cond), condToExpr(parent))
        }
        val newEn = condToExpr(fullEnStack)
        Some( MemRead(symbol, desc, address, newEn, note) )
      }
      case MemWrite(desc, address, source, mask, note) => {
        val enStack = calcDiff(cmdScope.condStack)
        def wrapCmd(cmd: CmdHW, enclosing: OpenCondition): CmdHW = enclosing match {
          case NoCond => cmd
          case OpenWhen(cond, parent)           => wrapCmd(WhenHW(cond, cmd, NOPHW, passNote), parent)
          case OpenElse(OpenWhen(cond, parent)) => wrapCmd(WhenHW(cond, NOPHW, cmd, passNote), parent)
        }
        val newCmd = wrapCmd(cmd, enStack)
          // TODO could defer emission and attempt emit at end after smashing together ones
          //   with equivalent addresses
        Some( newCmd )
      } 
      case _ => Some( CmdERROR(s"Unexpected Command for MemJournal: $cmd", cmd.note) )
    }
    def resolve: Iterable[CmdHW] = None
  }
}
