package gama
package internal
package journal

final class Journal {
  private val _entries = scala.collection.mutable.ListBuffer.empty[Entry]
  def entries: List[Entry] = _entries.toList

  def append(in: Entry): Unit = _entries.append(in)
}

object EmptyJournal {
  def apply(): Journal = new Journal
}
