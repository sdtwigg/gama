package gama
import internal._

object Reg {
  def apply[D<:Data](model: D)(implicit em: EnclosingModule): D = {
    val created = model.copy.rebind(RegSpell(em))
    em.getActiveJournal.append(CreateReg(created))
    created
  }
}

