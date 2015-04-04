package gama
package internal
package reader

import scala.collection.{immutable=>immutable}

sealed abstract class FoldedJournalReader extends BaseJournalReader {
  def parseJournal(entries: immutable.Seq[JournalEntry]): String = {
    ensureNamed(entries)
    val filteredEntries: immutable.Seq[JournalEntry] = entries flatMap(entry => entry match {
      case CreateOp(opdesc) if opdesc.retVal.namePriority.get <= NameFromMath => None
      case CreateAccessor(accdesc) if accdesc.retVal.namePriority.get <= NameFromMath => None
      case other => Some(other)
    })
    "{\n" +
    (filteredEntries flatMap(entry => parseJournalEntry(entry).split("\n")) map("  " + _) mkString("\n")) +
    "\n}"
  }
  def ensureNamed(entries: immutable.Seq[JournalEntry]): Unit = {
    def check(target: Nameable, tempprefix: String): Option[Tuple2[Nameable,String]] = {
      target.name match {
        case Some(_) => None
        case None    => Some((target, tempprefix))
      }
    }
    val itemsToTempName: Iterable[Tuple2[Nameable,String]] = entries flatMap(entry => entry match {
      // Determine which entries need named
      case CreateOp(opdesc)        => None // fold later
      case CreateWire(data)        => check(data,"W")
      case CreateReg(data)         => check(data,"R")
      case CreateAccessor(accdesc) => None // fold later
      case CreateModule(module)    => check(module, "M")
      case Conditionally(_,_,_) => None // recall: will recursively see
      case DataTransfer(_,_) => None
    })
    itemsToTempName.zipWithIndex.foreach(_ match {
      case ((target: Nameable, prefix: String), idx: Int) => {
        target.name = (s"${prefix}${idx}", NameFromTemp)
      }
    })
    // Folding pass
    entries foreach((entry: JournalEntry) => entry match {
      // Determine which entries need named
      case CreateOp(opdesc) if(opdesc.retVal.name.isEmpty) => {
        opdesc.retVal.name = (emitOpDesc(opdesc), NameFromMath)
      }
      case CreateAccessor(accdesc) if(accdesc.retVal.name.isEmpty) => {
        accdesc.retVal.name = (emitAccDesc(accdesc), NameFromMath)
      }
      case _ =>
    })
  }

  def emitRef(data: Data): String  = data.name.getOrElse("$$$$BADREF$$$$")
  def emitModuleInst(module: Module[_<:Data]): String = 
   s"${module.name.get}: ${HL.GREEN}${module.getClass.getName}${HL.RESET}"
}

object FoldedJournalReader {
  object Colorful    extends FoldedJournalReader {def HL = Highlighters.Colorful}
  object NonColorful extends FoldedJournalReader {def HL = Highlighters.NonColorful}
}
