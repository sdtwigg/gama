package gama
package internal

import scala.collection.{immutable=>immutable}

sealed abstract class RawJournalReader extends BaseJournalReader {
  def parseJournal(entries: immutable.Seq[JournalEntry]): String = {
    "{\n" +
    (entries flatMap(entry => parseJournalEntry(entry).split("\n")) map("  " + _) mkString("\n")) +
    "\n}"
  }

  def emitRef(data: Data): String = data.name.getOrElse(data.toString)
  def emitModuleInst(module: Module[_<:Data]): String = 
    module.name.getOrElse(module.toString)
}

object RawJournalReader {
  object Colorful    extends RawJournalReader {def HL = Highlighters.Colorful}
  object NonColorful extends RawJournalReader {def HL = Highlighters.NonColorful}
}
