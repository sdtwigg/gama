package gama
package internal

object WireInternals {
  def apply[D<:Data](model: D, em: EnclosingModule): D = {
    Desc.generate(model.copy.rebind(WireSpell(em)))(rv => WireDesc(rv, em))
  }
}

trait WireDescImpl[+T<:Data] {
  self: WireDesc[T] =>
    def validateRetVal(): Unit = NodeCheck.assertWireNode(retVal)
    def genJournalEntry = Some(CreateWire(this))
}
