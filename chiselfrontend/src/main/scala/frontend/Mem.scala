package gama
package frontend

import implementation._

// Activate Scala Language Features
import scala.language.experimental.macros
import implementation.macrodefs.{TransformMacro => XFORM}


object Mem {
  // External API
  def apply[D<:Data](model: D, depth: Int): Mem[D] = macro XFORM.doMem.xform[D]

  // external->internal API
  def doMem[D<:Data](model: D, depth: Int, storer: Storable[D], info: EnclosureInfo): Mem[D] = {
    val newmemory = new Mem(model.copy.rebind(MemSpecSpell(info.em)), info.em.clock, depth, storer, info)
    info.em.getActiveJournal.append(journal.CreateMem(newmemory))

    newmemory
  }
}

@annotation.implicitNotFound("Masked memory writes cannot be done on data of type ${D}, can only be done on subtypes of Digital.")
sealed trait MemMaskable[-D<:Data]
object MemMaskable {
  implicit object DigitalMaskable extends MemMaskable[Digital]
}

final class Mem[D<:Data] private (protected[gama] val elemType: D, protected[gama] val clock: Clock, val depth: Int, storer: Storable[D], protected[gama] val info: EnclosureInfo) extends MemAccessible[D] with Nameable {
  def collection = this
  
  // External API
  def write(addr: UIntLike, source: Data): Unit = macro XFORM.doMemWrite.twoarg
  def write(addr: UIntLike, source: Data, mask: Digital): Unit = macro XFORM.doMemWrite.threearg
  // TODO: CONSIDER: Note how doMemWrite will introduce an implicit
  //   is this OK?
  
  // external->internal API
  def doMemWrite(addr: UIntLike, source: D, acc_info: EnclosureInfo): Unit = {
    if(acc_info.em != info.em) { throw CrossedMemoryAccessException(acc_info.em, info.em) }
    info.em.getActiveJournal.append(
      implementation.journal.JMemWrite(this, addr, source, None, acc_info)
    )
  }
  def doMemWrite(addr: UIntLike, source: D, mask: Digital, acc_info: EnclosureInfo)(implicit canMask: MemMaskable[D]): Unit = {
    if(acc_info.em != info.em) { throw CrossedMemoryAccessException(acc_info.em, info.em) }
    info.em.getActiveJournal.append(
      implementation.journal.JMemWrite(this, addr, source, Some(mask), acc_info)
    )
  }

  protected[gama] def lookupIsConnectable(selector: UIntLike): Boolean = true
  // TODO: REMOVE WHEN ACCESSIBLE REFACTORED? probably no, Accessible works w Mem now
  // TODO: CONSIDER: Return false so writes can only be done through write call. Otherwise can do myMemOfBundle(myAddr).myBundleField := myUpdate, which is strange.
  // However, that may be a good story for masked writes??

  def propogateName(newname: NameTree, newsource: NameSource): Unit = {} 

}
