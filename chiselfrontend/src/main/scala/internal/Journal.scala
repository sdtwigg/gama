package gama
package internal

sealed trait JournalEntry
case class CreateOp(opdesc: OpDesc) extends JournalEntry
case class CreateWire(wiredesc: WireDesc[_<:Data]) extends JournalEntry
case class CreateReg(regdesc: RegDesc[_<:Data]) extends JournalEntry
case class CreateAccessor(accdesc: AccessorDesc[_<:Data]) extends JournalEntry
case class CreateModule(module: Module[_<:Data]) extends JournalEntry
case class Conditionally(cond: Bool, tc: Journal, fc: Journal) extends JournalEntry {NodeCheck.assertSynthesizable(cond)}
case class ConnectData(sink: Sink[Data], source: Source[Data]) extends JournalEntry {
  NodeCheck.assertConnectable(sink.data)
  NodeCheck.assertSynthesizable(source.data)
}

final class Journal {
  private val _entries = scala.collection.mutable.ListBuffer.empty[JournalEntry]
  def entries: List[JournalEntry] = _entries.toList

  def append(in: JournalEntry): Unit = _entries.append(in)
}

object EmptyJournal {
  def apply(): Journal = new Journal
}
