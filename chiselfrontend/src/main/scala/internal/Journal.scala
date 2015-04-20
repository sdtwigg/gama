package gama
package internal

import scala.language.existentials

sealed trait JournalEntry
case class CreateOp(opdesc: OpDesc) extends JournalEntry
case class CreateWire(wiredesc: WireDesc[_<:Data]) extends JournalEntry
case class CreateReg(regdesc: RegDesc[_<:Data]) extends JournalEntry
case class CreateAccessor(accdesc: AccessorDesc[_<:Data]) extends JournalEntry
case class CreateExtract(extdesc: ExtractDesc) extends JournalEntry
case class CreateMem(mem: Mem[_<:Data]) extends JournalEntry
case class CreateModule(module: Module[_<:Data]) extends JournalEntry
case class AddBlock(code: Journal) extends JournalEntry
case class Conditionally(cond: Bool, tc: Journal, fc: Journal) extends JournalEntry {NodeCheck.assertSynthesizable(cond)}
case class ConnectData(sink: Sink[Data], source: Source[Data], details: ConnectDetails) extends JournalEntry {
  NodeCheck.assertConnectable(sink.data)
  NodeCheck.assertSynthesizable(source.data)
}
case class BiConnectData(left: Left[Data], right: Right[Data], details: BiConnectDetails) extends JournalEntry {
  NodeCheck.assertConnectable(left.data)
  NodeCheck.assertConnectable(right.data)
}

final class Journal {
  private val _entries = scala.collection.mutable.ListBuffer.empty[JournalEntry]
  def entries: List[JournalEntry] = _entries.toList

  def append(in: JournalEntry): Unit = _entries.append(in)
}

object EmptyJournal {
  def apply(): Journal = new Journal
}
