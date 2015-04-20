package gama
package internal
package reader

import scala.collection.{immutable=>immutable}

trait JournalReader {
  def parseModule(module: Module[_<:Data]): String
  def parseJournal(entries: immutable.Seq[JournalEntry]): String
  def parseJournal(journal: Journal): String = parseJournal(journal.entries)
}

abstract class BaseJournalReader extends JournalReader {
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
    module.io.forceSetName(NameTerm("io"), NameFromIO, true)
    ensureAllChildrenNamed(module)
    module.children.foreach(child =>
      child.io.forceSetName(NameField(child.name.getOrElse(NameUNKNOWN), "io"), NameFromIO, true)
    )
    try {
      val ioType = emitType(module.io)
      val body = parseJournal(module.getActiveJournal)
      s"${HL.CYAN}module${HL.RESET} ${module.getClass.getName}(${HL.GREEN}${ioType}${HL.RESET})${body}"
    } finally {
      // wipeout names so know if future clients forget to adjust them
      module.io.forceSetName(NameUNKNOWN, NameFromIO, true)
      module.children.foreach(_.io.forceSetName(NameUNKNOWN, NameFromIO, true))
    }
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

  def parseJournalEntry(entry: JournalEntry): String = {
    entry match {
      case CreateOp(opdesc) =>
        s"${HL.CYAN}const${HL.RESET} ${emitRefType(opdesc.retVal)} = ${emitOpDesc(opdesc)}"
      case CreateWire(wiredesc) =>
        s"${HL.CYAN}wire${HL.RESET}  ${emitRefType(wiredesc.retVal)}"
      case CreateReg(regdesc) =>
        s"${HL.CYAN}reg${HL.RESET}   ${emitRefType(regdesc.retVal)}"
      case CreateAccessor(accdesc) =>
        s"${HL.CYAN}acc${HL.RESET}   ${emitRefType(accdesc.retVal)} = ${emitAccDesc(accdesc)}"
      case CreateExtract(extdesc) =>
        s"${HL.CYAN}extr${HL.RESET}  ${emitRefType(extdesc.retVal)} = ${emitExtDesc(extdesc)}"
      case CreateMem(mem) =>
        s"${HL.CYAN}mem${HL.RESET}   ${emitMemDetails(mem)}"
      case CreateModule(module) =>
        s"${HL.CYAN}inst${HL.RESET}  ${emitModuleInst(module)}"
      case AddBlock(journal) =>
        s"${parseJournal(journal)}"
      case Conditionally(cond, tc, fc) =>
        s"${HL.CYAN}when${HL.RESET}(${emitRef(cond)}) ${parseJournal(tc)} ${HL.CYAN}else${HL.RESET} ${parseJournal(fc)}"
      case ConnectData(Sink(sink), Source(source), details) =>
        s"${emitRef(sink)} := ${emitRef(source)} ${HL.BLUE}${emitConnectDetails(details)}${HL.RESET}"
      case BiConnectData(Left(left), Right(right), details) =>
        s"${emitRef(left)} <-> ${emitRef(right)} ${HL.BLUE}${emitBiConnectDetails(details)}${HL.RESET}"
    }
  }
  def emitConnectDetails(details: ConnectDetails): String = details match {
    case ConnectAll => "<ALL>"
    case ConnectVec(elemdetails) => s"${emitConnectDetails(elemdetails)}*"
    case ConnectTuple(fields) => "(" +
      (fields map ({case (field, subd) => s"$field:${emitConnectDetails(subd)}"}) mkString(", ")) +
      ")"
  }
  def emitBiConnectDetails(details: BiConnectDetails): String = details match {
    case BiConnectToLeft  => "<<=="
    case BiConnectToRight => "==>>"
    case BiConnectVec(elemdetails) => s"${emitBiConnectDetails(elemdetails)}*"
    case BiConnectTuple(fields) => "(" +
      (fields map ({case (field, subd) => s"$field:${emitBiConnectDetails(subd)}"}) mkString(", ")) +
      ")"
  }
  def emitMemDetails(mem: Mem[_<:Data]): String = 
    s"${emitName(mem.name)}[${mem.depth}]: ${HL.GREEN}${emitType(mem.elemType)}${HL.RESET}"
  def emitModuleInst(module: Module[_<:Data]): String

  def emitRef(data: Data): String
  def emitType(data: Data): String = data match {
    case elem: Element => (s"${emitIODirection(elem)}${emitNodeStore(elem.node.storage)}")
    case vec: Vec[_]   => (s"${emitType(vec.elemType)}[${vec.size}]")
    case hwt: HardwareTuple =>
      "{" +
      (hwt.subfields_ordered map({case (sf: String, elem: Data) => s"${sf}: ${emitType(elem)}"}) mkString(", ")) +
      "}"
  }
  def emitIODirection(elem: Element): String = elem.node match {
    case PortNode(_, direction, _) => direction match {
      case DirectionIO.Input  => "input "
      case DirectionIO.Output => "output "
    }
    case _ => ""
  }
  def emitNodeStore(store: NodeStore): String = store match {
    case rb: RawBits => {
      val simpleWidth = rb.width.map(_.toString).getOrElse("?")
      s"${rb.getClass.getSimpleName}(${simpleWidth})"
    }
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
      
      case OpToUInt => "toUInt"
      case OpToSInt => "toSInt"
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
    case mem: MemAccessible[_] => (emitName(mem.collection.name)) // assured mem.collection is a Mem[_]
  }

  def emitName(name: Option[NameTree]): String = parseNameTree(name.getOrElse(NameUNKNOWN))
  def parseNameTree(name: NameTree): String = name match {
    case NameTerm(identifier) => identifier
    case NameField(source, field) => s"${parseNameTree(source)}.${field}"
    case NameIndex(source, index) => s"${parseNameTree(source)}(${index})"

    case NameLit(litdesc) => parseLitDesc(litdesc)
    case NameUnnamedOp(opdesc) => "$$$$UNHANDLED" + opdesc.toString 

    case NameUNKNOWN => "$$$$UNKNOWN$$$$"
  }
  
  def parseLitDesc[D<:Data](litdesc: LitDesc[D]): String = parseLitMap(litdesc.litMap)
  def parseLitMap[D<:Data](litmap: LitMap[D]): String = litmap match {
    case BoolLitMap(value) => HL.RED + (if(value) "UBits(1,1)/*true*/)" else "UBits(0,1)/*false*/") + HL.RESET
    case UIntLitMap(value, width) => {
      val w = width.map(_.toString).getOrElse("?")
      s"${HL.RED}UBits($value, $w)${HL.RESET}"
    }
    case SIntLitMap(value, width) => {
      val w = width.map(_.toString).getOrElse("?")
      s"${HL.RED}SBits($value, $w)${HL.RESET}"
    }
    case VecLitMap(elemmaps) => {
      "{"+ elemmaps.map(emap=>parseLitMap(emap)).mkString(",") + "}"
    }
  }
}

