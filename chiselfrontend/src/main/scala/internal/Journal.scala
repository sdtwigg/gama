package gama
package internal

sealed trait JournalEntry
case class CreateOp(opdesc: OpDesc) extends JournalEntry {NodeCheck.assertOpNode(opdesc.retVal)}
case class CreateWire(data: Data)   extends JournalEntry {NodeCheck.assertWireNode(data)}
case class CreateReg(data: Data)    extends JournalEntry {NodeCheck.assertRegNode(data)}
case class CreateAccessor(accdesc: AccessorDesc[_<:Data]) extends JournalEntry {NodeCheck.assertAccessorNode(accdesc.retVal)}
case class CreateModule(module: Module[_<:Data]) extends JournalEntry
case class Conditionally(cond: Bool, tc: Journal, fc: Journal) extends JournalEntry {NodeCheck.assertSynthesizable(cond)}
case class DataTransfer(source: Data, sink: Data) extends JournalEntry {
  NodeCheck.assertConnectable(sink)
  NodeCheck.assertSynthesizable(source)
}

final class Journal {
  private val _entries = scala.collection.mutable.ListBuffer.empty[JournalEntry]
  def entries: List[JournalEntry] = _entries.toList

  def append(in: JournalEntry): Unit = _entries.append(in)
}

object EmptyJournal {
  def apply(): Journal = new Journal
}
