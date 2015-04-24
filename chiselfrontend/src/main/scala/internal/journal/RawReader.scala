package gama
package internal
package journal

import scala.collection.{immutable=>immutable}

sealed abstract class RawReader extends BaseReader {
  def parseJournal(entries: immutable.Seq[Entry]): String = {
    (entries flatMap(entry => parseEntry(entry).split("\n")) map("  " + _) mkString("{\n","\n","\n}"))
  }

  // These aren't really readable but this is for raw debugging so
  //   not quite the point
  def emitRef(data: Data): String =
    data.name.map(n=>emitName(Some(n))).getOrElse(data.toString)
  def emitModuleInst(module: Module[_<:Data]): String = 
    module.name.map(n=>emitName(Some(n))).getOrElse(module.toString)
}

object RawReader {
  object Colorful    extends RawReader {def HL = Highlighters.Colorful}
  object NonColorful extends RawReader {def HL = Highlighters.NonColorful}
}
