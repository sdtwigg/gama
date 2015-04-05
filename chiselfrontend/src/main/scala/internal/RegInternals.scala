package gama
package internal 
object RegInternals {
  def apply[D<:Data](model: D, em: EnclosingModule): D = {
    Desc.generate(model.copy.rebind(RegSpell(em)))(rv => RegDesc(rv, em))
  }
}

trait RegDescImpl[+T<:Data] {
  self: RegDesc[T] =>
    def validateRetVal(): Unit = NodeCheck.assertRegNode(retVal)
    def genJournalEntry = CreateReg(this)
}
