package gama
package internal

import scala.collection.{immutable=>immutable}

trait JournalReader {
  def parseJournal(entries: immutable.Seq[JournalEntry]): String
  def apply(journal: Journal): String = parseJournal(journal.entries)
}

abstract class BaseJournalReader extends JournalReader {
  def HL: Highlighter

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
        s"${HL.CYAN}when${HL.RESET}(${emitRef(cond)}) ${apply(tc)} ${HL.CYAN}else${HL.RESET} ${apply(fc)}"
      case DataTransfer(source, sink) =>
        s"${emitRef(sink)} := ${emitRef(source)}"
    }
  }
  def emitModuleInst(module: Module[_<:Data]): String

  def emitRef(data: Data): String
  def emitType(data: Data): String = data match {
    case elem: Element => (s"${emitNodeStore(elem.node.storage)}")
    case vec: Vec[_]   => (s"${emitType(vec.elemType)}[${vec.size}]")
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

