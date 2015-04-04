package gama
package internal

import scala.collection.{immutable=>immutable}

sealed abstract class AllJournalReader extends BaseJournalReader {
  def parseJournal(entries: immutable.Seq[JournalEntry]): String = {
    ensureNamed(entries)
    "{\n" +
    (entries flatMap(entry => parseJournalEntry(entry).split("\n")) map("  " + _) mkString("\n")) +
    "\n}"
  }
  def ensureNamed(entries: immutable.Seq[JournalEntry]): Unit = {
    ???
  }

  def emitRef(data: Data): String  = data.name.get
}

object AllJournalReader {
  object Colorful    extends AllJournalReader {def HL = Highlighters.Colorful}
  object NonColorful extends AllJournalReader {def HL = Highlighters.NonColorful}
}
