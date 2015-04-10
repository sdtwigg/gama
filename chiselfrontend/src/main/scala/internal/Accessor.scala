package gama
package internal

sealed trait Accessible[+D<:Data] {
  def collection: Nameable // Needed so readers can actually figure out what the collection they desire to access actually is, also it should probably be nameable so accessors have meaning....
  // internal API
  protected[gama] def lookupIsConnectable(selector: UIntLike): Boolean
  protected[gama] def elemType: D
  // external API
  def lookup(selector: UIntLike)(implicit em: EnclosingModule): D

  def apply(selector: UIntLike, em: EnclosingModule): D
  def apply(arg0: UIntLike): D
  // implementation details
  protected[gama] def makeAccessor(selector: UIntLike, em: EnclosingModule): D = {
    val spell: NodeSpell[AccessorNode] =
      if(lookupIsConnectable(selector)) ConnectableAccessorSpell(em)
      else NonConnectableAccessorSpell(em)
    Desc.generate(elemType.copy.rebind(spell))(rv =>
      AccessorDesc[D](this, selector, rv, em)
    )
  }
}

trait VecAccessible[D<:Data] extends Accessible[D] {
  self: Vec[D] =>
  def collection: Vec[D]
  
  def lookup(selector: UIntLike)(implicit em: EnclosingModule): D =
    makeAccessor(selector, em)

  def apply(selector: UIntLike, em: EnclosingModule): D = lookup(selector)(em)
  import scala.language.experimental.macros
  def apply(arg0: UIntLike): D = macro macroDef.transformapply1
}

case class CrossedMemoryAccessException(acc_em: EnclosingModule, mem_em: EnclosingModule) 
 extends ChiselException(s"Attempted to create Accessor in $acc_em but memory resides in $mem_em")
trait MemAccessible[D<:Data] extends Accessible[D] {
  self: Mem[D] =>
  def collection: Mem[D]
  
  def lookup(selector: UIntLike)(implicit acc_em: EnclosingModule): D = {
    if(acc_em != collection.em) { throw CrossedMemoryAccessException(acc_em, em) }
    makeAccessor(selector, em)
  }

  def apply(selector: UIntLike, em: EnclosingModule): D = lookup(selector)(em)
  import scala.language.experimental.macros
  def apply(arg0: UIntLike): D = macro macroDef.transformapply1
}

trait AccessorDescImpl[+T<:Data] {
  self: AccessorDesc[T] =>
    def validateRetVal(): Unit = NodeCheck.assertAccessorNode(retVal)
    def genJournalEntry = Some(CreateAccessor(this))
}
