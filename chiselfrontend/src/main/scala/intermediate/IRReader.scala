package gama
package intermediate

case class IRReaderOptions(emitNotes: Boolean, emitExprTypes: Boolean)

abstract class IRReader(options: IRReaderOptions) {
  def HL: Highlighter
  def parseElaboratedModule(module: ElaboratedModule, enclosingCircuit: Option[ElaboratedCircuit]): String = {
    implicit val circuitLUT = enclosingCircuit
    s"Module(${HL.CYAN}IO${HL.RESET}: ${HL.GREEN}${parseType(module.io)}${HL.RESET}, ${parseCmdHW(module.body)})"
  }

  def parseCmdHW(cmd: CmdHW)(implicit circuitLUT: Option[ElaboratedCircuit]): String = cmd match {
    case WireDecl(symbol, note) => s"${HL.CYAN}wire ${HL.RESET} ${emitFullSymbol(symbol)}  ${emitGamaNote(note)}"
    case RegDecl(symbol, clock, reset, note)  => {
      val cinfo: String = s"${HL.CYAN}clock${HL.RESET} = ${parseExpr(clock)}"
      val rinfo: String = reset match {
        case Some((ren, rval)) => s"${HL.CYAN}and reset${HL.RESET}(en = ${parseExpr(ren)}, rval = ${parseExpr(rval)})"
        case None => ""
      }
      s"${HL.CYAN}reg  ${HL.RESET} ${emitFullSymbol(symbol)} ${HL.CYAN}with${HL.RESET} $cinfo $rinfo  ${emitGamaNote(note)}"
    }
    case ConstDecl(symbol, expr, note) =>
      s"${HL.CYAN}const${HL.RESET} ${emitFullSymbol(symbol)} = ${parseExpr(expr)}  ${emitGamaNote(note)}"
    case AliasDecl(symbol, expr, note) =>
      s"${HL.CYAN}alias${HL.RESET} ${emitFullSymbol(symbol)} = ${parseExpr(expr)}  ${emitGamaNote(note)}"

    case BlockHW(cmds, note) =>
      if(cmds.isEmpty) "{}"
      else 
        (cmds flatMap(cmd => parseCmdHW(cmd).split("\n")) map("  " + _) mkString("{\n","\n","\n}"))
    case WhenHW(cond, tc, fc, note) =>
      s"${HL.CYAN}when${HL.RESET}(${parseExpr(cond)}) ${parseCmdHW(tc)} ${HL.CYAN}else${HL.RESET} ${parseCmdHW(fc)}  ${emitGamaNote(note)}"

    case ConnectStmt(sink, source, details, note) =>
      s"${parseExpr(sink)} := ${parseExpr(source)} ${HL.YELLOW}${parseConnectDetails(details)}${HL.RESET}  ${emitGamaNote(note)}"
    case BiConnectStmt(left, right, details, note) =>
      s"${parseExpr(left)} <-> ${parseExpr(right)} ${HL.YELLOW}${parseBiConnectDetails(details)}${HL.RESET}  ${emitGamaNote(note)}"
    
    case MemDecl(desc, clock, note) => {
      val cinfo: String = s"${HL.CYAN}with clock${HL.RESET} = ${parseExpr(clock)}"
      s"${HL.CYAN}mem${HL.RESET} ${emitMemName(desc)} = ${HL.CYAN}MEM${HL.RESET}(${desc.depth}, ${HL.GREEN}${parseType(desc.mType)}${HL.RESET}) $cinfo  ${emitGamaNote(note)}"
    }
    case MemRead(symbol, mem, address, en, note) =>
      s"${HL.CYAN}mem read${HL.RESET} ${emitFullSymbol(symbol)} = ${emitMemName(mem)}(${parseExpr(address)}) ${HL.CYAN}with en${HL.RESET} = ${parseExpr(en)}  ${emitGamaNote(note)}"
    case MemWrite(desc, address, source, mask, note) => {
      val minfo: String = mask match {
        case Some(mask) => s"${HL.CYAN}with mask${HL.RESET} = ${parseExpr(mask)}"
        case None => ""
      }
      s"${HL.CYAN}mem write${HL.RESET} ${emitMemName(desc)}(${parseExpr(address)}) = ${parseExpr(source)} ${minfo} ${emitGamaNote(note)}"
    }

    case SubModuleDecl(details, smptr, note) => {
      val submoduletype = circuitLUT.map(lut => if(smptr>=0 && smptr < lut.modules.size) s"${lut.modules(smptr).selftype} " else "").getOrElse("")
      s"${HL.CYAN}inst${HL.RESET} ${emitModName(details)}: ${HL.GREEN}${submoduletype}($smptr)${HL.RESET}, ${HL.CYAN}IO${HL.RESET}: ${HL.GREEN}${parseType(details.ioType)}${HL.RESET}  ${emitGamaNote(note)}"
    }
    case CmdERROR(message, note) => s"${HL.RED}!!ERROR!!${HL.RESET}: $message ${emitGamaNote(note)}"
  }
  def emitGamaNote(note: GamaNote): String = note match {
    case GamaNote(GTSourceUserspace(UserspaceInfo(file, line))) if options.emitNotes =>
      s"${HL.BLUE}/* ${file} @ ${line} */${HL.RESET}"
    case GamaNote(GTSourcePass(passid)) if options.emitNotes =>
      s"${HL.BLUE}/* $passid */${HL.RESET}"
    case _ => ""
  }

  def emitModName(desc: ModuleSub): String = {
    val name = desc.identifier.getOrElse("")
    s"$name${HL.WHITE}#S${desc.modid}${HL.RESET}"
  }
  def emitMemName(desc: MemDesc): String = {
    val name = desc.identifier.getOrElse("")
    s"$name${HL.WHITE}#M${desc.memid}${HL.RESET}"
  }
  def emitSymbol(symbol: RefSymbol): String = {
    val name = symbol.identifier.getOrElse("")
    s"$name${HL.WHITE}#D${symbol.symbol}${HL.RESET}"
  }
  def emitFullSymbol(symbol: RefSymbol): String = {
    val name = symbol.identifier.getOrElse("")
    s"${emitSymbol(symbol)}: ${HL.GREEN}${parseType(symbol.rType)}${HL.RESET}"
  }
  
  def parseExpr(expr: ExprHW): String = {
    def parseUnary(opid: OpIdUnary, input: ExprHW, note: GamaNote): String = {
      val opname = opid match {
        case OpIDENT  => ""
        
        case OpAsUInt => "asUInt"
        case OpAsSInt => "asSInt"
        
        case OpNot    => "not"
        
        case OpXorRed => "xorR"
      }
      s"$opname(${parseExpr(input)})"
    }
    def parseBinary(opid: OpIdBinary, left: ExprHW, right: ExprHW, note: GamaNote): String = {
      def infix(opname: String)   = s"(${parseExpr(left)} $opname ${parseExpr(right)})"
      def postfix(opname: String) = s"$opname(${parseExpr(left)}, ${parseExpr(right)})"
      opid match {
        case OpPlus  => infix("+")
        case OpSubt  => infix("-")
        case OpMult  => infix("*")
        case OpDiv   => infix("/")
        case OpMod   => infix("%")

        case OpAnd   => infix("&")
        case OpOr    => infix("|")
        case OpXor   => infix("^")
        
        case OpPadTo => postfix("pad")
        case OpCat   => infix("##")
        case OpLShft => infix("<<")
        case OpRShft => infix(">>")
        
        case OpEqual => infix("===")
        case OpNotEq => infix("!==")
        case OpLess  => infix("<")
        case OpLeEq  => infix("<=")
        case OpGrt   => infix(">")
        case OpGrEq  => infix(">=")
      }
    }
    val exprstring = expr match {
      case symbol @ RefSymbol(_,_,_,_) => emitSymbol(symbol)
      case ExprUnary(op, target, _, note) => parseUnary(op, target, note)
      case ExprBinary(op, left, right, _, note) => parseBinary(op, left, right, note)
      case ExprMux(cond, tc, fc, _, note) => s"((${parseExpr(cond)}) ? (${parseExpr(tc)}) : (${parseExpr(fc)}))"
      case ExprLit(litvalue, _, note) => s"${HL.RED}${parseLitTree(litvalue)}${HL.RESET}"

      case RefIO(details, note) => s"${parseModRef(details)}->${HL.CYAN}IO${HL.RESET}"
      case RefMSelect(mem, selector, note)    => s"${emitMemName(mem)}#${HL.CYAN}MEM${HL.RESET}(${parseExpr(selector)})"
      case RefVIndex(parent, index, note)     => s"${parseExpr(parent)}($index)"
      case RefVSelect(parent, selector, note) => s"${parseExpr(parent)}(${parseExpr(selector)})"
      case RefTLookup(source, field, note)    => s"${parseExpr(source)}.$field"
      case RefExtract(source, left_pos, right_pos, note) => s"${parseExpr(source)}($left_pos,$right_pos)"

      case RefExprERROR(cause) => "$$$$RefExprERROR: " + cause + " $$$$"
    }
    if(options.emitExprTypes) s"$exprstring: ${HL.GREEN}${parseType(expr.rType)}${HL.RESET}"
    else exprstring // emitExprTypes is slightly verbose because it emits all references
  }

  def parseModRef(in: ModuleRef) = in match {
    case ModuleThis(_) => s"${HL.CYAN}this${HL.RESET}"
    case mod @ ModuleSub(_, _, _) => emitModName(mod)
  }

  def parseLitTree(litvalue: LitTree): String = litvalue match {
    case LitRawBits(value, width, signed) => s"$value{${width}${if(signed) 'S' else 'U'}}"
    case LitVec(elements) => elements map(elem => parseLitTree(elem)) mkString("{",",","}")
    case LitTuple(fields) => ???
  }
  
  def parseType(typehw: TypeHW): String = typehw match {
    case PrimitiveNode(storage) => parseNodeStore(storage)
    case PrimitivePort(storage, dir) => s"${parseDirectionIO(dir)} ${parseNodeStore(storage)}"
    case TupleHW(fields) =>
      fields.toSeq.sortBy(_._1) map({
        case (field, elem) => s"${field}: ${parseType(elem)}"
      }) mkString("{",", ","}")
    case VecHW(depth, elemtype) => s"${parseType(elemtype)}[${depth}]"
    case TypeHWUNKNOWN => "$$$$UNKNOWNTYPE$$$$"
  }
  def parseNodeStore(store: NodeStore): String = store match {
    case rb: RawDigital => {
      val simpleWidth = rb.width.map(_.toString).getOrElse("?")
      s"${rb.getClass.getSimpleName}(${simpleWidth})"
    }
    case ClockNS => "ClockNS"
  }
  def parseDirectionIO(dir: DirectionIO): String = dir match {
    case DirectionIO.Input => "input"
    case DirectionIO.Output => "output"
  }
  
  def parseConnectDetails(details: ConnectDetails): String = details match {
    case ConnectAll => "<ALL>"
    case ConnectVec(_,elemdetails) => s"${parseConnectDetails(elemdetails)}*"
    case ConnectTuple(fields) =>
      (fields.toSeq.sortBy(_._1) map ({
        case (field, subd) => s"$field:${parseConnectDetails(subd)}"
      }) mkString("(",", ",")"))
  }
  def parseBiConnectDetails(details: BiConnectDetails): String = details match {
    case BiConnectToLeft  => "<<=="
    case BiConnectToRight => "==>>"
    case BiConnectVec(_, elemdetails) => s"${parseBiConnectDetails(elemdetails)}*"
    case BiConnectTuple(fields) =>
      (fields map ({case (field, subd) => s"$field:${parseBiConnectDetails(subd)}"}) mkString("(",", ",")"))
  }
  
  def parseTT(in: TypeTrace): String = in match {
    case TTStart(expr) => s"{${parseExpr(expr)}}"
    case TTIndexALL(tt) => s"${parseTT(tt)}.*"
    case TTField(tt, field) => s"${parseTT(tt)}.$field"
  }
}

object IRReader {
  case class Colorful(options: IRReaderOptions)    extends IRReader(options) {def HL = Highlighters.Colorful}
  case class NonColorful(options: IRReaderOptions) extends IRReader(options) {def HL = Highlighters.NonColorful}
}
