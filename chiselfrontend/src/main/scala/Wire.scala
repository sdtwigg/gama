package gama
import internal._

object Wire {
  def apply[D<:Data](model: D)(implicit em: EnclosingModule): D = {
    val created = model.copy.rebind(WireSpell(em))

    em.getActiveJournal.append(CreateWire(created))

    created
  }
}

