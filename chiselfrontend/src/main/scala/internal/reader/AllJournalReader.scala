package gama
package internal
package reader

import scala.collection.{immutable=>immutable}

sealed abstract class AllJournalReader extends BaseJournalReader {
  def parseJournal(entries: immutable.Seq[JournalEntry]): String = {
    ensureNamed(entries)
    "{\n" +
    (entries flatMap(entry => parseJournalEntry(entry).split("\n")) map("  " + _) mkString("\n")) +
    "\n}"
  }
  def ensureNamed(entries: immutable.Seq[JournalEntry]): Unit = {
    def check(target: Nameable, tempprefix: String): Option[Tuple2[Nameable,String]] = {
      target.name match {
        case Some(_) => None
        case None    => Some((target, tempprefix))
      }
    }
    val itemsToName: Iterable[Tuple2[Nameable,String]] = entries flatMap((entry: JournalEntry) => entry match {
      // Determine which entries need named
      case CreateOp(opdesc)        => check(opdesc.retVal,"T")
      case CreateWire(data)        => check(data,"W")
      case CreateReg(data)         => check(data,"R")
      case CreateAccessor(accdesc) => check(accdesc.retVal,"A")
      case CreateModule(module)    => None // handled above
      case Conditionally(_,_,_) => None // recall: will recursively see
      case DataTransfer(_,_) => None
    })
    itemsToName.zipWithIndex.foreach(_ match {
      case ((target: Nameable, prefix: String), idx: Int) => {
        target.name = (s"${prefix}${idx}", NameFromTemp)
      }
    })
  }

  def emitRef(data: Data): String  = data.name.get
  def emitModuleInst(module: Module[_<:Data]): String = 
   s"${module.name.get}: ${HL.GREEN}${module.getClass.getName}${HL.RESET}"
}

object AllJournalReader {
  object Colorful    extends AllJournalReader {def HL = Highlighters.Colorful}
  object NonColorful extends AllJournalReader {def HL = Highlighters.NonColorful}
}
