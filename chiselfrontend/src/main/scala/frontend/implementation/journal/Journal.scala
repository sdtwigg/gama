package gama
package frontend
package implementation
package journal

final class Journal private {
  private val _entries = scala.collection.mutable.ListBuffer.empty[Entry]
  def entries: List[Entry] = _entries.toList

  def append(in: Entry): Unit = _entries.append(in)
  protected[gama] def insertAfter(in: Entry, posmarker: Entry): Unit = {
    val found_idx = _entries.indexWhere(_==posmarker)
    assert(found_idx >= 0, s"Internal Error: Could not find $posmarker in Journal during insertAfter")
    val ins_idx = found_idx + 1
    _entries.insert(ins_idx, in)
  }
}

object Journal {
  def empty(): Journal = new Journal
}
