package gama
package internal

object Port {
 protected[gama] def apply[D<: Data](model: D, em: EnclosingModule): D = {
    model.rebind(PortSpell(em))
    // TODO: not copying is intentional here but tricky...
    //   evaluate later
  }
}

trait PortDescImpl[+T<:Data] {
  self: PortDesc[T] =>
    def validateRetVal(): Unit = NodeCheck.assertPortNode(retVal)
    def genJournalEntry = None
}

