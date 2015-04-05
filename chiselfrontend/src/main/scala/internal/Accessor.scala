package gama
package internal

trait Accessible[+D<:Data] extends Data {
  def lookupIsConnectable(selector: UInt): Boolean
  def elemType: D
  def lookup(selector: UInt)(implicit em: EnclosingModule): D = {
    val spell: NodeSpell[AccessorNode] =
      if(lookupIsConnectable(selector)) ConnectableAccessorSpell(em)
      else NonConnectableAccessorSpell(em)
    val retVal = elemType.copy.rebind(spell)
    val newAccessor = AccessorDesc[D](this, selector, retVal, em)
    retVal.descRef = newAccessor
    retVal
  }
  def apply(selector: UInt)(implicit em: EnclosingModule): D = lookup(selector)
}

trait AccessorDescImpl[+T<:Data] {
  self: AccessorDesc[T] =>
    em.getActiveJournal.append(CreateAccessor(this))
}
