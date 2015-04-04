package gama
package internal

trait Accessible[+D<:Data] extends Data {
  def lookupCheck(selector: UInt): Unit
  def elemType: D
  def lookup(selector: UInt)(implicit em: EnclosingModule): D = {
    lookupCheck(selector)
    val retVal = elemType.copy.rebind(AccessorSpell, em)
    val newAccessor = AccessorDesc[D](this, selector, retVal, em)
    retVal.descRef = newAccessor
    retVal
  }
  def apply(selector: UInt)(implicit em: EnclosingModule): D = lookup(selector)
}

abstract class AccessorDescImpl[+T<:Data] {
  self: AccessorDesc[T] =>
    em.getActiveJournal.append(CreateAccessor(this))
}
