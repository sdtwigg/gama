package gama
package frontend
package implementation
package journal

import scala.collection.{immutable=>immutable}

trait Reader {
  def parseModule(module: Module[_<:Data]): String
  def parseJournal(entries: immutable.Seq[Entry]): String
  def parseJournal(journal: Journal): String = parseJournal(journal.entries)
}

abstract class BaseReader extends Reader {
  // TODO: SPLIT THIS INTO MULTIPLE TRAITS
  def HL: Highlighter
  
  def parseCircuit(topModule: Module[_<:Data]): Seq[String] = {
    val setupQueue = scala.collection.mutable.Queue[Module[_<:Data]](topModule)
    val workList = scala.collection.mutable.ListBuffer.empty[Module[_<:Data]]
    while(setupQueue.nonEmpty) {
      val work = setupQueue.dequeue
      setupQueue.enqueue(work.children:_*)
      workList.append(work)
    }
    workList.toList.map(parseModule(_))
  }
  
  def parseModule(module: Module[_<:Data]): String = {
    ensureAllChildrenNamed(module)
    val module_name = module.name.getOrElse(NameUNKNOWN)
    val module_src  = module.nameSource.getOrElse(NameFromInit)
    module.forceSetName(NameTerm(s"${HL.CYAN}this${HL.RESET}"), module_src, true)
      // TODO: kinda hacky although readers are mainly for debugging anyway...
    try{
      val ioType = emitType(module.io) // TODO: Make full IO type?
      val body = parseJournal(module.getActiveJournal)
      s"${HL.CYAN}module${HL.RESET} ${module.getClass.getName}(${HL.GREEN}${ioType}${HL.RESET})${body}"
    } finally {module.forceSetName(module_name, module_src, true)} // restore true io name
  }
  def ensureAllChildrenNamed(module: Module[_<:Data]): Unit = {
    val childrenToName: Iterable[Nameable] = module.children flatMap(child => {
      child.name match {
        case None    => Some(child)
        case Some(_) => None
      }
    })
    childrenToName.zipWithIndex.foreach(_ match {
      case (target: Nameable, idx: Int) => {
        target.checkedSetName(NameTerm(s"M${idx}"), NameFromTemp, true)
      }
    })
  }

  def parseEntry(entry: Entry): String = {
    entry match {
      case CreateOp(opdesc) =>
        s"${HL.CYAN}const${HL.RESET} ${emitRefType(opdesc.retVal)} = ${emitOpDesc(opdesc)}  ${emitEncInfo(opdesc.info)}"
      case CreateWire(wiredesc) =>
        s"${HL.CYAN}wire${HL.RESET}  ${emitRefType(wiredesc.retVal)}  ${emitEncInfo(wiredesc.info)}"
      case CreateReg(regdesc) => {
        val cinfo: String = s"=> ${HL.CYAN}clock${HL.RESET} = ${emitRef(regdesc.clock)}"
        val rinfo: String = regdesc.reset match {
          case Some((ren, rval)) => s"${HL.CYAN}reset${HL.RESET} en = ${emitRef(ren)}, rval = ${emitRef(rval)}"
          case None => ""
        }
        s"${HL.CYAN}reg${HL.RESET}   ${emitRefType(regdesc.retVal)} => $cinfo $rinfo ${emitEncInfo(regdesc.info)}"
      }
      case CreateAccessor(accdesc) =>
        s"${HL.CYAN}acc${HL.RESET}   ${emitRefType(accdesc.retVal)} = ${emitAccDesc(accdesc)}  ${emitEncInfo(accdesc.info)}"
      case CreateExtract(extdesc) =>
        s"${HL.CYAN}extr${HL.RESET}  ${emitRefType(extdesc.retVal)} = ${emitExtDesc(extdesc)}  ${emitEncInfo(extdesc.info)}"
      case CreateMem(mem) =>
        s"${HL.CYAN}mem${HL.RESET}   ${emitMemDetails(mem)} => ${HL.CYAN}clock${HL.RESET} = ${emitRef(mem.clock)} ${emitEncInfo(mem.info)}"
      case CreateModule(module) =>
        s"${HL.CYAN}inst${HL.RESET}  ${emitModuleInst(module)}"
      case JMemWrite(mem, selector, source, mask, info) => {
        val minfo: String = mask match {
          case Some(mask) => "with mask = ${emitRef(mask)}"
          case None => ""
        }
        s"${HL.CYAN}mem write${HL.RESET} ${emitName(mem)}(${emitRef(selector)}) = ${emitRef(source)} $minfo ${emitEncInfo(info)}"
      }
      case AddBlock(journal) =>
        s"${parseJournal(journal)}"
      case Conditionally(cond, tc, fc) =>
        s"${HL.CYAN}when${HL.RESET}(${emitRef(cond)}) ${parseJournal(tc)} ${HL.CYAN}else${HL.RESET} ${parseJournal(fc)}"
      case ConnectData(Sink(sink), Source(source), details, info) =>
        s"${emitRef(sink)} := ${emitRef(source)} ${HL.YELLOW}${emitConnectDetails(details)}${HL.RESET}  ${emitEncInfo(info)}"
      case BiConnectData(Left(left), Right(right), details, info) =>
        s"${emitRef(left)} <-> ${emitRef(right)} ${HL.YELLOW}${emitBiConnectDetails(details)}${HL.RESET}  ${emitEncInfo(info)}"
    }
  }
  def emitEncInfo(info: EnclosureInfo): String = {
    info.debug.map(uinfo => {
      s"${HL.BLUE}/* ${uinfo.file} @ ${uinfo.line} */${HL.RESET}"
    }).getOrElse("")
  }

  def emitConnectDetails(details: ConnectDetails): String = details match {
    case ConnectAll => "<ALL>"
    case ConnectVec(_,elemdetails) => s"${emitConnectDetails(elemdetails)}*"
    case ConnectTuple(fields) =>
      (fields map ({case (field, subd) => s"$field:${emitConnectDetails(subd)}"}) mkString("(",", ",")"))
  }
  def emitBiConnectDetails(details: BiConnectDetails): String = details match {
    case BiConnectToLeft  => "<<=="
    case BiConnectToRight => "==>>"
    case BiConnectVec(_, elemdetails) => s"${emitBiConnectDetails(elemdetails)}*"
    case BiConnectTuple(fields) =>
      (fields map ({case (field, subd) => s"$field:${emitBiConnectDetails(subd)}"}) mkString("(",", ",")"))
  }
  def emitMemDetails(mem: Mem[_<:Data]): String = 
    s"${emitName(mem)}[${mem.depth}]: ${HL.GREEN}${emitType(mem.elemType)}${HL.RESET}"
  def emitModuleInst(module: Module[_<:Data]): String

  def emitRef(data: Data): String
  def emitType(data: Data): String = data match {
    case elem: Element => (s"${emitIODirection(elem)}${emitNodeStore(elem.node.storage)}")
    case vec: Vec[_]   => (s"${emitType(vec.elemType)}[${vec.size}]")
    case hwt: HardwareTuple =>
      (hwt.subfields_ordered map({case (sf: String, elem: Data) => s"${sf}: ${emitType(elem)}"}) mkString("{",", ","}"))
  }
  def emitIODirection(elem: Element): String = elem.node match {
    case PortNode(_, direction, _) => direction match {
      case DirectionIO.Input  => "input "
      case DirectionIO.Output => "output "
    }
    case _ => ""
  }
  def emitNodeStore(store: NodeStore): String = store match {
    case rb: RawDigital => {
      val simpleWidth = rb.width.map(_.toString).getOrElse("?")
      s"${rb.getClass.getSimpleName}(${simpleWidth})"
    }
    case ClockNS => "ClockNS"
  }
  def emitRefType(data: Data): String = s"${emitRef(data)}: ${HL.GREEN}${emitType(data)}${HL.RESET}"

  def emitOpDesc(op: OpDesc): String = op match {
    case UnaryOpDesc(op, input, _,_)          => emitUnaryOp(op, input)
    case BinaryOpDesc(op, (left, right), _,_) => emitBinaryOp(op, left, right)
    case MuxDesc(cond, tc, fc, _,_)           => (s"((${emitRef(cond)}) ? (${emitRef(tc)}) : (${emitRef(fc)}))")
  }

  def emitUnaryOp(opid: OpIdUnary, input: Data): String = {
    val opname = opid match {
      case OpIDENT  => ""
      
      case OpAsUInt => "asUInt"
      case OpAsSInt => "asSInt"
      
      case OpNot    => "not"
      
      case OpXorRed => "xorR"
    }
    s"$opname(${emitRef(input)})"
  }
  def emitBinaryOp(opid: OpIdBinary, left: Element, right: Element): String = {
    def infix(opname: String)   = s"(${emitRef(left)} $opname ${emitRef(right)})"
    def postfix(opname: String) = s"$opname(${emitRef(left)}, ${emitRef(right)})"
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
  
  def emitExtDesc(extdesc: ExtractDesc): String =
    s"${emitRef(extdesc.base)}(${extdesc.left_pos},${extdesc.right_pos})"
  def emitAccDesc(accdesc: AccessorDesc[_<:Data]): String =
    s"${emitAccessibleRef(accdesc.accRef)}(${emitRef(accdesc.selector)})"
  def emitAccessibleRef(acc: Accessible[_<:Data]): String = acc match {
    case vec: VecAccessible[_] => (emitRef(vec.collection))
    case mem: MemAccessible[_] => (emitName(mem.collection)) // assured mem.collection is a Mem[_]
  }

  def emitName(target: Nameable): String = parseNameTree(target.name.getOrElse(NameUNKNOWN))
  def parseNameTree(name: NameTree): String = name match {
    case NameTerm(identifier) => identifier
    case NameIO(source, field) => s"${emitName(source)}->$field"
    case NameField(source, field) => s"${emitName(source)}.${field}"
    case NameIndex(source, index) => s"${emitName(source)}(${index})"

    case NameLit(litdesc) => parseLitDesc(litdesc)

    case NameUNKNOWN => "$$$$UNKNOWN$$$$"
  }
  
  def parseLitDesc[D<:Data](litdesc: LitDesc[D]): String = parseLitMap(litdesc.litMap)
  def parseLitMap[D<:Data](litmap: LitMap[D]): String = litmap match {
    case BoolLitMap(value) => HL.RED + (if(value) "UBits(1,1)/*true*/)" else "UBits(0,1)/*false*/") + HL.RESET
    case UIntLitMap(value, width) => s"${HL.RED}UBits($value, $width)${HL.RESET}"
    case SIntLitMap(value, width) => s"${HL.RED}SBits($value, $width)${HL.RESET}"
    case VecLitMap(elemmaps) => {
      elemmaps.map(emap=>parseLitMap(emap)).mkString("{",",","}")
    }
  }
}

