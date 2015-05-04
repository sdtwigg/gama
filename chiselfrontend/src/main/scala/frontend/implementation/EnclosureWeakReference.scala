package gama
package frontend
package implementation

abstract class EnclosureWeakReference[A<:AnyRef](incomingReference: A) {
  protected[this] val lostLinkDetails: String
  
  private[this] val enclosureRef: scala.ref.WeakReference[A] = scala.ref.WeakReference(incomingReference)
  def enclosure: A = enclosureRef.get.getOrElse(throw DesignLostException(lostLinkDetails))
}
