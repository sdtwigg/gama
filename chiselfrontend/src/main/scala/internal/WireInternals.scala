package gama
package internal

object WireInternals {
  def apply[D<:Data](model: D, info: EnclosureInfo): D = {
    Desc.generate(model.copy.rebind(WireSpell(info.em)))(rv => WireDesc(rv, info))
  }
}

trait WireDescImpl[+T<:Data] {
  self: WireDesc[T] =>
    def validateRetVal(): Unit = NodeCheck.assertWireNode(retVal)
    def genJournalEntry = Some(CreateWire(this))
}
