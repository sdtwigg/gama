package gama
package frontend
package implementation
package journal

final class Journal private {
  private val _entries = scala.collection.mutable.ListBuffer.empty[Entry]
  def entries: List[Entry] = _entries.toList

  def append(in: Entry): Unit = _entries.append(in)
}

object Journal {
  def empty(): Journal = new Journal
}
