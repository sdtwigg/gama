package gama
package internal

sealed trait Accessible[+D<:Data] {
  def collection: Nameable // Needed so readers can actually figure out what the collection they desire to access actually is, also it should probably be nameable so accessors have meaning....
  // internal API
  protected[gama] def lookupIsConnectable(selector: UIntLike): Boolean
  protected[gama] def elemType: D

  // external -> internal API
  def doLookup(selector: UIntLike, info: EnclosureInfo): D
  
  // external API
  import scala.language.experimental.macros
  def lookup(selector: UIntLike): D = macro macrodefs.TransformMacro.doLookup.onearg
  def read(selector: UIntLike): D   = macro macrodefs.TransformMacro.doLookup.onearg
  def apply(selector: UIntLike): D  = macro macrodefs.TransformMacro.doLookup.onearg

  // implementation details
  protected[gama] def makeAccessor(selector: UIntLike, info: EnclosureInfo): D = {
    val spell: NodeSpell[AccessorNode] =
      if(lookupIsConnectable(selector)) ConnectableAccessorSpell(info.em)
      else NonConnectableAccessorSpell(info.em)
    Desc.generate(elemType.copy.rebind(spell))(rv =>
      AccessorDesc[D](this, selector, rv, info)
    )
  }
}

trait VecAccessible[D<:Data] extends Accessible[D] {
  self: Vec[D] =>
  def collection: Vec[D]
  
  def doLookup(selector: UIntLike, info: EnclosureInfo): D =
    makeAccessor(selector, info)
}

case class CrossedMemoryAccessException(acc_em: EnclosingModule, mem_em: EnclosingModule) 
 extends ChiselException(s"Attempted to create Accessor in $acc_em but memory resides in $mem_em")
trait MemAccessible[D<:Data] extends Accessible[D] {
  self: Mem[D] =>
  def collection: Mem[D]
  
  def doLookup(selector: UIntLike, info: EnclosureInfo): D = {
    if(info.em != collection.em) { throw CrossedMemoryAccessException(info.em, em) }
    makeAccessor(selector, info)
  }
}

trait AccessorDescImpl[+T<:Data] {
  self: AccessorDesc[T] =>
    def validateRetVal(): Unit = NodeCheck.assertAccessorNode(retVal)
    def genJournalEntry = Some(CreateAccessor(this))
}
