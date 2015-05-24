package gama
package frontend
package implementation

object RegInternals {
  def apply[D<:Data](model: D, clock: Clock, reset: Option[Tuple2[Bool, D]], info: EnclosureInfo): D = {
    Desc.generate(model.copy.rebind(RegSpell(info.em)))(rv => RegDesc(clock, reset, rv, info))
  }
}

trait RegDescImpl[+T<:Data] {
  self: RegDesc[T] =>
    def validateRetVal(): Unit = NodeCheck.assertRegNode(retVal)
    def genJournalEntry = Some(journal.CreateReg(this))
}

