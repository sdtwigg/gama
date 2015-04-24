package gama
package internal 
object RegInternals {
  def apply[D<:Data](model: D, info: EnclosureInfo): D = {
    Desc.generate(model.copy.rebind(RegSpell(info.em)))(rv => RegDesc(rv, info))
  }
}

trait RegDescImpl[+T<:Data] {
  self: RegDesc[T] =>
    def validateRetVal(): Unit = NodeCheck.assertRegNode(retVal)
    def genJournalEntry = Some(journal.CreateReg(this))
}

