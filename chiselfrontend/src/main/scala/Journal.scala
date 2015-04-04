package gama
package internal

sealed trait JournalEntry
case class CreateOp(opdesc: OpDesc) extends JournalEntry
case class CreateWire(data: Data) extends JournalEntry {
  data.nodes.foreach(node => require(node.isInstanceOf[WireNode]))
}
case class CreateReg(data: Data) extends JournalEntry {
  data.nodes.foreach(node => require(node.isInstanceOf[RegNode]))
}
case class CreateAccessor(accdesc: AccessorDesc[_<:Data]) extends JournalEntry
case class CreateModule(module: Module[_<:Data]) extends JournalEntry
case class Conditionally(cond: Bool, tc: Journal, fc: Journal) extends JournalEntry {
  require(cond.node.isInstanceOf[Synthesizable])
}
case class NodeAssign(source: Data, sink: Data) extends JournalEntry 

final class Journal {
  private val _entries = scala.collection.mutable.ListBuffer.empty[JournalEntry]
  def entries: List[JournalEntry] = _entries.toList

  def append(in: JournalEntry): Unit = _entries.append(in)
}

object EmptyJournal {
  def apply(): Journal = new Journal
}
