package gama
package internal

sealed trait OpJournalEntry
case class CreateNode(target: Node) extends OpJournalEntry
case class NodeAssign(target: Node, source: Node) extends OpJournalEntry
case class Conditionally(cond: Node, tc: OpJournal, fc: OpJournal) extends OpJournalEntry

class OpJournal

object EmptyOpJournal {
  def apply() = new OpJournal
}
