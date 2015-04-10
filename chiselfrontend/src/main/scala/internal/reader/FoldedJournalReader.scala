package gama
package internal
package reader

import scala.collection.{immutable=>immutable}

sealed abstract class FoldedJournalReader extends BaseJournalReader {
  def parseJournal(entries: immutable.Seq[JournalEntry]): String = {
    ensureNamed(entries)
    
    // Filter out ops that are just arithmetic expressions (and thus folded into other things)
    //   Take everything else, including entries with no name (which is problematic at this phase)
    def descFilter: Desc=>Boolean = desc => desc.retVal.nameSource.map(_ > NameFromMath).getOrElse(true)
    val filteredEntries: immutable.Seq[JournalEntry] = entries filter (entry => entry match {
      case CreateOp(opdesc) => descFilter(opdesc)
      case CreateAccessor(accdesc) => descFilter(accdesc)
      case other => true
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
      case CreateWire(wiredesc)    => check(wiredesc.retVal,"W")
      case CreateReg(regdesc)      => check(regdesc.retVal,"R")
      case CreateAccessor(accdesc) => None // fold later
      case CreateMem(mem)          => check(mem, "mem")
      case CreateModule(module)    => check(module, "M")
      case AddExecBlock(_)    => None
      case Conditionally(_,_,_) => None // recall: will recursively see
      case ConnectData(_,_) => None
    })
    itemsToTempName.zipWithIndex.foreach(_ match {
      case ((target: Nameable, prefix: String), idx: Int) => {
        target.checkedSetName(NameTerm(s"${prefix}${idx}"), NameFromTemp, true)
      }
    })
    // Folding pass
    entries foreach((entry: JournalEntry) => entry match {
      // Determine which entries need named
      case CreateOp(opdesc) if(opdesc.retVal.name.isEmpty) => {
        opdesc.retVal.checkedSetName(NameTerm(emitOpDesc(opdesc)), NameFromMath, true)
      }
      case CreateAccessor(accdesc) if(accdesc.retVal.name.isEmpty) => {
        accdesc.retVal.checkedSetName(NameTerm(emitAccDesc(accdesc)), NameFromMath, true)
      }
      case _ =>
    })
  }

  def emitRef(data: Data): String  = emitName(data.name)
  def emitModuleInst(module: Module[_<:Data]): String = 
   s"${emitName(module.name)}: ${HL.GREEN}${module.getClass.getName}${HL.RESET}"
}

object FoldedJournalReader {
  object Colorful    extends FoldedJournalReader {def HL = Highlighters.Colorful}
  object NonColorful extends FoldedJournalReader {def HL = Highlighters.NonColorful}
}
