package gama
package internal

sealed trait OpJournalEntry
case class CreateNode(target: Synthesizable) extends OpJournalEntry
case class NodeAssign(source: Node, sink: Node) extends OpJournalEntry
case class Conditionally(cond: Node, tc: OpJournal, fc: OpJournal) extends OpJournalEntry

sealed class OpJournal {
  private val entries = scala.collection.mutable.ListBuffer.empty[OpJournalEntry]

  def append(in: OpJournalEntry): Unit = entries.append(in)
  override def toString = "OpJournal{%s}".format(entries.mkString("\n"))
}

object EmptyOpJournal {
  def apply(): OpJournal = new OpJournal
}
