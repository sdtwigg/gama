package gama
package internal

trait Accessible[+D<:Data] extends Data {
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
  def apply(selector: UInt)(implicit em: EnclosingModule): D = lookup(selector)
}

trait AccessorDescImpl[+T<:Data] {
  self: AccessorDesc[T] =>
    def validateRetVal(): Unit = NodeCheck.assertAccessorNode(retVal)
    def genJournalEntry = CreateAccessor(this)
}