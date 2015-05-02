package gama
package internal
package frontend

trait IRReader {
  def HL: Highlighter

  def parseCmdHW(cmd: CmdHW): String = cmd match {
    case WireDecl(symbol) => s"${HL.CYAN}wire ${HL.RESET} ${emitFullSymbol(symbol)}"
    case RegDecl(symbol, reset)  => {
      val rinfo: String = reset match {
        case Some((ren, rval)) => s"=> ${HL.CYAN}reset${HL.RESET} en = ${parseExpr(ren)}, rval = ${parseExpr(rval)}"
        case None => ""
      }
      s"${HL.CYAN}reg  ${HL.RESET} ${emitFullSymbol(symbol)} $rinfo"
    }
    case ConstDecl(symbol, expr) =>
      s"${HL.CYAN}const${HL.RESET} ${emitFullSymbol(symbol)} = ${parseExpr(expr)}"
    case AliasDecl(symbol, expr) =>
      s"${HL.CYAN}alias${HL.RESET} ${emitFullSymbol(symbol)} = ${parseExpr(expr)}"

    case BlockHW(cmds) =>
      if(cmds.isEmpty) "{}"
      else 
        (cmds flatMap(cmd => parseCmdHW(cmd).split("\n")) map("  " + _) mkString("{\n","\n","\n}"))
    case WhenHW(cond, tc, fc) =>
      s"${HL.CYAN}when${HL.RESET}(${parseExpr(cond)}) ${parseCmdHW(tc)} ${HL.CYAN}else${HL.RESET} ${parseCmdHW(fc)}"

    case MemDecl(desc) => {
      val name = desc.identifier.getOrElse("")
      s"${HL.CYAN}mem${HL.RESET} ${emitMemName(desc)} = MEM(${desc.depth}, ${HL.GREEN}${parseType(desc.sourceType)}${HL.RESET})"
    }

    case ConnectStmt(sink, source, details) =>
      s"${parseExpr(sink)} := ${parseExpr(source)} ${HL.YELLOW}${parseConnectDetails(details)}${HL.RESET}"
    case BiConnectStmt(left, right, details) =>
      s"${parseExpr(left)} <-> ${parseExpr(right)} ${HL.YELLOW}${parseBiConnectDetails(details)}${HL.RESET}"

    case SubModuleDecl(details, placeholder) =>
      s"${HL.CYAN}inst${HL.RESET} ${emitModName(details)}: ${HL.GREEN}$placeholder${HL.RESET}"
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
    def parseUnary(opid: OpIdUnary, input: ExprHW): String = {
      val opname = opid match {
        case OpIDENT  => ""
        
        case OpToUInt => "toUInt"
        case OpToSInt => "toSInt"
        case OpAsUInt => "asUInt"
        case OpAsSInt => "asSInt"
        
        case OpNot    => "not"
        
        case OpXorRed => "xorR"
      }
      s"$opname(${parseExpr(input)})"
    }
    def parseBinary(opid: OpIdBinary, left: ExprHW, right: ExprHW): String = {
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
    expr match {
      case symbol @ RefSymbol(_,_,_) => emitSymbol(symbol)
      case ExprUnary(op, target, _) => parseUnary(op, target)
      case ExprBinary(op, left, right, _) => parseBinary(op, left, right)
      case ExprMux(cond, tc, fc, _) => s"((${parseExpr(cond)}) ? (${parseExpr(tc)}) : (${parseExpr(fc)}))"
      case ExprLit(litvalue, _) => s"${HL.RED}${parseLitTree(litvalue)}${HL.RESET}"

      case RefIO(details) => s"${parseModRef(details)}->${HL.CYAN}io${HL.RESET}"
      case RefMSelect(mem, selector)    => s"${emitMemName(mem)}(${parseExpr(selector)})"
      case RefVIndex(parent, index)     => s"${parseExpr(parent)}($index)"
      case RefVSelect(parent, selector) => s"${parseExpr(parent)}(${parseExpr(selector)})"
      case RefTLookup(source, field)    => s"${parseExpr(source)}.$field"
      case RefExtract(source, left_pos, right_pos, _) => s"${parseExpr(source)}($left_pos,$right_pos)"

      case RefExprERROR(cause) => "$$$$RefExprERROR: " + cause + " $$$$"
    }
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
    case rb: RawBits => {
      val simpleWidth = rb.width.map(_.toString).getOrElse("?")
      s"${rb.getClass.getSimpleName}(${simpleWidth})"
    }
  }
  def parseDirectionIO(dir: DirectionIO): String = dir match {
    case DirectionIO.Input => "input"
    case DirectionIO.Output => "output"
  }
  
  def parseConnectDetails(details: ConnectDetails): String = details match {
    case ConnectAll => "<ALL>"
    case ConnectVec(elemdetails) => s"${parseConnectDetails(elemdetails)}*"
    case ConnectTuple(fields) =>
      (fields.toSeq.sortBy(_._1) map ({
        case (field, subd) => s"$field:${parseConnectDetails(subd)}"
      }) mkString("(",", ",")"))
  }
  def parseBiConnectDetails(details: BiConnectDetails): String = details match {
    case BiConnectToLeft  => "<<=="
    case BiConnectToRight => "==>>"
    case BiConnectVec(elemdetails) => s"${parseBiConnectDetails(elemdetails)}*"
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
  object Colorful    extends IRReader {def HL = Highlighters.Colorful}
  object NonColorful extends IRReader {def HL = Highlighters.NonColorful}
}
