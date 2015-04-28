package gama
package internal
package journal

import scala.collection.{immutable=>immutable}

sealed abstract class AllReader extends BaseReader {
  def parseJournal(entries: immutable.Seq[Entry]): String = {
    ensureNamed(entries)
    if(entries.isEmpty) "{}"
    else
      (entries flatMap(entry => parseEntry(entry).split("\n")) map("  " + _) mkString("{\n", "\n", "\n}"))
  }
  def ensureNamed(entries: immutable.Seq[Entry]): Unit = {
    def check(target: Nameable, tempprefix: String): Option[Tuple2[Nameable,String]] = {
      target.name match {
        case Some(_) => None
        case None    => Some((target, tempprefix))
      }
    }
    val itemsToName: Iterable[Tuple2[Nameable,String]] = entries flatMap((entry: Entry) => entry match {
      // Determine which entries need named
      case CreateOp(opdesc)        => check(opdesc.retVal,"T")
      case CreateWire(wiredesc)    => check(wiredesc.retVal,"W")
      case CreateReg(regdesc)      => check(regdesc.retVal,"R")
      case CreateAccessor(accdesc) => check(accdesc.retVal,"A")
      case CreateExtract(extdesc)  => check(extdesc.retVal,"E")
      case CreateMem(mem)          => check(mem, "mem")
      case CreateModule(module)    => None // handled above
      case AddBlock(_)    => None
      case Conditionally(_,_,_) => None // recall: will recursively see
      case ConnectData(_,_,_,_) => None
      case BiConnectData(_,_,_,_) => None
    })
    itemsToName.zipWithIndex.foreach(_ match {
      case ((target: Nameable, prefix: String), idx: Int) => {
        target.checkedSetName(NameTerm(s"${prefix}${idx}"), NameFromTemp, true)
      }
    })
  }

  def emitRef(data: Data): String  = emitName(data)
  def emitModuleInst(module: Module[_<:Data]): String = 
   s"${emitName(module)}: ${HL.GREEN}${module.getClass.getName}${HL.RESET}"
}

object AllReader {
  object Colorful    extends AllReader {def HL = Highlighters.Colorful}
  object NonColorful extends AllReader {def HL = Highlighters.NonColorful}
}
