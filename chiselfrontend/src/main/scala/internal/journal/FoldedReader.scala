package gama
package internal
package journal

import scala.collection.{immutable=>immutable}

sealed abstract class FoldedReader extends BaseReader {
  def parseJournal(entries: immutable.Seq[Entry]): String = {
    ensureNamed(entries)
    
    // Filter out ops that are just arithmetic expressions (and thus folded into other things)
    //   Take everything else, including entries with no name (which is problematic at this phase)
    def descFilter: Desc=>Boolean = desc => desc.retVal.nameSource.map(_ > NameFromMath).getOrElse(true)
    val filteredEntries: immutable.Seq[Entry] = entries filter (entry => entry match {
      case CreateOp(opdesc)        => descFilter(opdesc)
      case CreateAccessor(accdesc) => descFilter(accdesc)
      case CreateExtract(extdesc)  => descFilter(extdesc)
      case other => true
    })
    if(filteredEntries.isEmpty) "{}"
    else
      (filteredEntries flatMap(entry => parseEntry(entry).split("\n")) map("  " + _) mkString("{\n","\n","\n}"))
  }
  def ensureNamed(entries: immutable.Seq[Entry]): Unit = {
    def hasName(target: Nameable): Boolean = {
      target.name match {
        case None | Some(NameUNKNOWN) => false
        case Some(_) => true
      }
    }
    def makeTemp(target: Nameable, tempprefix: String): Option[Tuple2[Nameable,String]] =
      if(hasName(target)) None else Some((target, tempprefix))

    val itemsToTempName: Iterable[Tuple2[Nameable,String]] = entries flatMap(entry => entry match {
      // Determine which entries need named
      case CreateOp(opdesc)        => None // fold later
      case CreateWire(wiredesc)    => makeTemp(wiredesc.retVal,"W")
      case CreateReg(regdesc)      => makeTemp(regdesc.retVal,"R")
      case CreateAccessor(accdesc) => None // fold later
      case CreateExtract(extdesc)  => None // fold later
      case CreateMem(mem)          => makeTemp(mem, "mem")
      case CreateModule(module)    => makeTemp(module, "M")
      case AddBlock(_)    => None
      case Conditionally(_,_,_) => None // recall: will recursively see
      case ConnectData(_,_,_,_) => None
      case BiConnectData(_,_,_,_) => None
    })
    itemsToTempName.zipWithIndex.foreach(_ match {
      case ((target: Nameable, prefix: String), idx: Int) => {
        target.checkedSetName(NameTerm(s"${prefix}${idx}"), NameFromTemp, true)
      }
    })
    // Folding pass
    entries foreach((entry: Entry) => entry match {
      // Determine which entries need named
      case CreateOp(opdesc) if(!hasName(opdesc.retVal)) => {
        opdesc.retVal.checkedSetName(NameTerm(emitOpDesc(opdesc)), NameFromMath, true)
      }
      case CreateAccessor(accdesc) if(!hasName(accdesc.retVal)) => {
        accdesc.retVal.checkedSetName(NameTerm(emitAccDesc(accdesc)), NameFromMath, true)
      }
      case CreateExtract(extdesc) if(!hasName(extdesc.retVal)) => {
        extdesc.retVal.checkedSetName(NameTerm(emitExtDesc(extdesc)), NameFromMath, true)
      }
      case _ =>
    })
  }

  def emitRef(data: Data): String  = emitName(data)
  def emitModuleInst(module: Module[_<:Data]): String = 
   s"${emitName(module)}: ${HL.GREEN}${module.getClass.getName}${HL.RESET}"
}

object FoldedReader {
  object Colorful    extends FoldedReader {def HL = Highlighters.Colorful}
  object NonColorful extends FoldedReader {def HL = Highlighters.NonColorful}
}
