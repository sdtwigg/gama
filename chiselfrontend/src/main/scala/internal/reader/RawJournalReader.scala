package gama
package internal
package reader

import scala.collection.{immutable=>immutable}

sealed abstract class RawJournalReader extends BaseJournalReader {
  def parseJournal(entries: immutable.Seq[JournalEntry]): String = {
    "{\n" +
    (entries flatMap(entry => parseJournalEntry(entry).split("\n")) map("  " + _) mkString("\n")) +
    "\n}"
  }

  // These aren't really readable but this is for raw debugging so
  //   not quite the point
  def emitRef(data: Data): String =
    data.name.map(n=>emitName(Some(n))).getOrElse(data.toString)
  def emitModuleInst(module: Module[_<:Data]): String = 
    module.name.map(n=>emitName(Some(n))).getOrElse(module.toString)
}

object RawJournalReader {
  object Colorful    extends RawJournalReader {def HL = Highlighters.Colorful}
  object NonColorful extends RawJournalReader {def HL = Highlighters.NonColorful}
}
