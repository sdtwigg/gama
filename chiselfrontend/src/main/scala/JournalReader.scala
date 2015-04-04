package gama
package internal

import scala.collection.{immutable=>immutable}

trait JournalReader {
  def parseModule(module: Module[_<:Data]): String
  def parseJournal(entries: immutable.Seq[JournalEntry]): String
  def parseJournal(journal: Journal): String = parseJournal(journal.entries)
}

abstract class BaseJournalReader extends JournalReader {
  def HL: Highlighter
  
  def parseModule(module: Module[_<:Data]): String = {
    module.io.name = ("io", NameOVERRIDE)
    ensureAllChildrenNamed(module)
    module.children.foreach(child => child.io.name = (s"${child.name.get}.io", NameOVERRIDE))
    try {
      val ioType = emitType(module.io)
      val body = parseJournal(module.getActiveJournal)
      s"${HL.CYAN}module${HL.RESET} ${module.getClass.getName}(${HL.GREEN}${ioType}${HL.RESET})${body}"
    } finally {
    module.io.name = ("$$$$UNDEFIO$$$$", NameOVERRIDE)
    module.children.foreach(child => child.io.name = (s"$$$$UNDEFIO$$$$", NameOVERRIDE))
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
        target.name = (s"M${idx}", NameFromTemp)
      }
    })
  }

  def parseJournalEntry(entry: JournalEntry): String = {
    entry match {
      case CreateOp(opdesc) =>
        s"${HL.CYAN}node${HL.RESET}  ${emitRefType(opdesc.retVal)} = ${emitOpDesc(opdesc)}"
      case CreateWire(data) =>
        s"${HL.CYAN}wire${HL.RESET}  ${emitRefType(data)}"
      case CreateReg(data) =>
        s"${HL.CYAN}reg${HL.RESET}   ${emitRefType(data)}"
      case CreateAccessor(accdesc) =>
        s"${HL.CYAN}acc${HL.RESET}   ${emitRefType(accdesc.retVal)} = ${emitAccDesc(accdesc)}"
      case CreateModule(module) =>
        s"${HL.CYAN}inst${HL.RESET}  ${emitModuleInst(module)}"
      case Conditionally(cond, tc, fc) =>
        s"${HL.CYAN}when${HL.RESET}(${emitRef(cond)}) ${parseJournal(tc)} ${HL.CYAN}else${HL.RESET} ${parseJournal(fc)}"
      case DataTransfer(source, sink) =>
        s"${emitRef(sink)} := ${emitRef(source)}"
    }
  }
  def emitModuleInst(module: Module[_<:Data]): String

  def emitRef(data: Data): String
  def emitType(data: Data): String = data match {
    case elem: Element => (s"${emitNodeStore(elem.node.storage)}")
    case vec: Vec[_]   => (s"${emitType(vec.elemType)}[${vec.size}]")
    case hwt: HardwareTuple =>
      "{" +
      (hwt.subfields map({case (sf: String, elem: Data) => s"${sf}: ${emitType(elem)}"}) mkString(", ")) +
      "}"
  }
  def emitNodeStore(store: NodeStore): String = store match {
    case rb: RawBits => {
      val simpleWidth = rb.width.map(_.toString).getOrElse("?")
      s"${rb.getClass.getSimpleName}(${simpleWidth})"
    }
  }
  def emitRefType(data: Data): String = s"${emitRef(data)}: ${HL.GREEN}${emitType(data)}${HL.RESET}"

  def emitOpDesc(op: OpDesc): String = op match {
    case UnaryOpDesc(op, input, _,_)          => (s"${op.shorthand}(${emitRef(input)})")
    case BinaryOpDesc(op, (left, right), _,_) => (s"(${emitRef(left)} ${op.shorthand} ${emitRef(right)})")
    case ExtractOpDesc(input, lp, rp, _,_)    => (s"${emitRef(input)}(${lp}, ${rp})")
    case MuxDesc(cond, tc, fc, _,_)           => (s"((${emitRef(cond)}) ? (${emitRef(tc)}) : (${emitRef(fc)}))")
  }
  def emitAccDesc(accdesc: AccessorDesc[_<:Data]): String =
    s"${emitRef(accdesc.collection)}(${emitRef(accdesc.selector)})"

}

