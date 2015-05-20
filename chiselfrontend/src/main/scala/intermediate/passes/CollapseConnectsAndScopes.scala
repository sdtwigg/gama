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

  protected def constructCT(ref: ExprHW): Option[CollapseTarget] = ref match {
    case symbol @ RefSymbol(_,_,_,_) => Some( CTSymbol(symbol) )
    case RefTLookup(RefIO(mod,_),field,_) => Some( CTIO(mod, field) )
    case RefExtract(source,_,_,_) => constructCT(source)
    case _ => None
  }

  sealed trait OpenCondition
  case object NoCond extends OpenCondition
  case class OpenWhen(cond: ExprHW, parent: OpenCondition) extends OpenCondition
  case class OpenElse(owhen: OpenWhen) extends OpenCondition
  // Instead of updating the condition on all open journals, just sync accessed journal as new condition encountered
  //   by closing old conditions to ancestor (and then opening up new ones until the current condition opened)

  def transform(target: ElaboratedModule): ElaboratedModule = {
    // TODO: Assumes InternalAggregateExplode, CircuitFlattenIO run (thus very minimal presence of aggregates)
    //       Assumes ExpandM/VSelect run (no RefVSelect or RefMSelect exist in the tree)
    //       Assumes TightenConnect run (so muxes can safely be created between similar connect statements)
    val masterScope = new ScopeContainer(None, NoCond)
    masterScope.injectTopIO(target.io)
    val newbody = BlockHW(masterScope.process(Some(target.body)).toList, passNote)
    target.copy(body = newbody)
  }

  final class ScopeContainer(parent: Option[ScopeContainer], val condStack: OpenCondition) {
    private[this] val scopedJournals = LHMap.empty[CollapseTarget, CmdJournal]
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

    protected def lookup(key: CollapseTarget): Option[CmdJournal] =
      scopedJournals.get(key) orElse parent.flatMap(_.lookup(key)) 

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

  sealed abstract class CmdJournal(scope: ScopeContainer) {
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
  sealed trait ConnectProcess {
    self: CmdJournal =>
    sealed trait JEntry
    case class JConnect(cmd: ConnectStmt) extends JEntry
    case class JWhen(cond: ExprHW, tc: ListB[JEntry], fc: ListB[JEntry], encCond: OpenCondition) extends JEntry {
      def tcCond = OpenWhen(cond, encCond)
      def fcCond = OpenElse(tcCond)
        // These are used for fast comparisons with a new commands scope
    }
    protected[this] val commands = ListB.empty[JEntry]
    private[this] def getBuffer(targetCond: OpenCondition): ListB[JEntry] = targetCond match {
      // Makes the buffer if needed!
      case NoCond => commands
      case OpenWhen(cond, parentCond) => {
        val parentBuffer = getBuffer(parentCond)
        parentBuffer.lastOption match {
          case Some( when @ JWhen(_, tc, _, _) ) if when.tcCond == targetCond => tc
          case _ => {
            val newWhen = JWhen(cond, ListB.empty, ListB.empty, parentCond)
            parentBuffer += newWhen
            newWhen.tc
          }
        }
      }
      case OpenElse(OpenWhen(cond, parentCond)) => {
        val parentBuffer = getBuffer(parentCond)
        parentBuffer.lastOption match {
          case Some( when @ JWhen(_, _, fc, _) ) if when.fcCond == targetCond => fc
          case _ => {
            val newWhen = JWhen(cond, ListB.empty, ListB.empty, parentCond)
            parentBuffer += newWhen
            newWhen.fc
          }
        }
      }
    }

    def process(cmd: CmdHW, cmdScope: ScopeContainer): Iterable[CmdHW] = cmd match {
      case cstmt @ ConnectStmt(sink, source, details, note) => {
        val trueCond = calcDiff(cmdScope.condStack)
        val targetBuffer = getBuffer(trueCond)
        targetBuffer += JConnect(cstmt)

        None
      }
      case _ => Some( CmdERROR(s"Unexpected Command for WireJournal: $cmd", cmd.note) )
    }
    /*
    def print_debug(target: CollapseTarget) = {
      val myIRReader = IRReader.Colorful(IRReaderOptions(emitNotes=true,emitExprTypes=false))
      def printJE(in: JEntry): String = in match {
        case JCmd(cmd) => myIRReader.parseCmdHW(cmd)(None)
        case JWhen(cond, tc, fc, _) => s"when(${myIRReader.parseExpr(cond)}) ${printLB(tc)} else ${printLB(fc)}"
      }
      def printLB(in: ListB[JEntry]): String = 
        (in flatMap(entry => printJE(entry).split("\n")) map("  " + _) mkString("{\n","\n","\n}"))
      println(s"Journal FOR $target")
      println(printLB(commands))
    }
    */
  }

  case class WireJournal(target: CollapseTarget, scope: ScopeContainer) extends CmdJournal(scope) with ConnectProcess {
    // translate when into muxes, needs defaults
    def resolve: Iterable[CmdHW] = {
      val sink: RefHW = target match {
        case CTSymbol(symbol) => symbol
        case CTIO(module, field) => RefTLookup(RefIO(module, passNote), field, passNote)
        case _ => RefExprERROR(s"Improper target for WireJournal: $target")
      }

      lazy val muxType = typePort2Node(sink.rType)
      def scopeResolve(commands: Iterable[JEntry], oldDefault: Option[ExprHW]): Option[ExprHW] = {
        commands.foldLeft(oldDefault)((current, entry) => entry match {
          case JConnect(ConnectStmt(RefExtract(csink, lp, rp, _), source, _, _))
            if constructCT(csink).map(_==target).getOrElse(false) => 
              asPrimitiveTypeHW(sink.rType).flatMap(getRawBitsInfo(_)) match {
                case None => throw new Exception(s"Internal Error: Wire with non-bit or non-primitive type (${sink.rType}) was extracted (and this has not been detected until far too late).")
                case Some((_ , None)) => Some(RefExprERROR("Failure to resolve because widths partially unknown"))
                case Some((sinkSigned, Some(sinkLength))) => {
                  // TODO: check source rType is sane? also, ideally would have a type inferer do the typing...
                  val  mask = ExprLitU( (BigInt(2) << lp) - (BigInt(1) << rp), sinkLength )
                  val rType = mask.rType
                  val update = ExprBinary(OpAnd, source, mask, rType, passNote)

                  val nmask = ExprUnary(OpNot, mask, rType, passNote)
                  val getCurrent = current.map(default => 
                    if(sinkSigned) ExprUnary(OpAsUInt, default, PrimitiveNode(UBits(Some(sinkLength))), passNote)
                    else default
                  ).getOrElse(RefExprERROR("Unknown Value: Extract requires default"))

                  val retain = ExprBinary(OpAnd, getCurrent, nmask, rType, passNote)

                  val merged = ExprBinary(OpOr, update, retain, rType, passNote)

                  val converted =
                    if(sinkSigned) ExprUnary(OpAsSInt, merged, PrimitiveNode(SBits(Some(sinkLength))), passNote)
                    else merged
                  Some( converted )
                }
              }
          case JConnect(ConnectStmt(csink, source, _, _))
            if constructCT(csink).map(_==target).getOrElse(false) =>
              Some(source)
          case JConnect(_) => throw new Exception(s"Internal Error: Bad connect $entry added to journal for $target")
          case JWhen(cond, tc, fc, _) => {
            val tcResult = scopeResolve(tc, current).getOrElse(RefExprERROR("Unknown Value for tc"))
            val fcResult = scopeResolve(fc, current).getOrElse(RefExprERROR("Unknown Value for fc"))
            Some(ExprMux(cond, tcResult, fcResult, muxType, passNote))
          }
        })
      }
      val newSource = scopeResolve(commands, None).getOrElse(RefExprERROR("Unknown Value"))
      // TODO: Could introduce a RandomBits LitTree and use that instead of RefExprERROR
      //       according to a CL option
      
      Some(ConnectStmt(sink, newSource, ConnectAll, passNote))
    }
  }
  case class RegJournal(target: CTSymbol, scope: ScopeContainer) extends CmdJournal(scope) with ConnectProcess {
    def resolve: Iterable[CmdHW] = {
      def scopeResolve(commands: Iterable[JEntry]): BlockHW =
        BlockHW(commands.map({
          case JConnect(cmd) => cmd
          case JWhen(cond, tc, fc, _) => {
            WhenHW(cond, scopeResolve(tc), scopeResolve(fc), passNote)
          }
        }).toList, passNote)
      Some(scopeResolve(commands))
      // TODO: Have this simplify when possible (but need to be careful of nested holes and subword updates
    }
  }
  case class MemJournal(target: CTMem, scope: ScopeContainer) extends CmdJournal(scope) {
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
