package gama
package internal

sealed trait Accessible[+D<:Data] {
  def collection: AnyRef // Needed so readers can actually figure out what the collection they desire to access actually is
    // TODO: Just force Nameable???
  def lookupIsConnectable(selector: UInt): Boolean
  def elemType: D
  def lookup(selector: UInt)(implicit em: EnclosingModule): D = {
    val spell: NodeSpell[AccessorNode] =
      if(lookupIsConnectable(selector)) ConnectableAccessorSpell(em)
      else NonConnectableAccessorSpell(em)
    Desc.generate(elemType.copy.rebind(spell))(rv =>
      AccessorDesc[D](this, selector, rv, em)
    )
  }

  def apply(selector: UInt, em: EnclosingModule): D = lookup(selector)(em)
  import scala.language.experimental.macros
  def apply(arg0: UInt): D = macro macroDef.transformapply1
}

trait VecAccessible[D<:Data] extends Accessible[D] {
  self: Vec[D] =>
  def collection: Vec[D]
}
trait MemAccessible[D<:Data] extends Accessible[D] {
  self: Mem[D] =>
  def collection: Mem[D]
}

trait AccessorDescImpl[+T<:Data] {
  self: AccessorDesc[T] =>
    def validateRetVal(): Unit = NodeCheck.assertAccessorNode(retVal)
    def genJournalEntry = Some(CreateAccessor(this))
}
