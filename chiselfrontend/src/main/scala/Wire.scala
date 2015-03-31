package gama
import internal._

class Connectable(storage: NodeStore, em: EnclosingModule) extends Synthesizable(storage, em)

protected[gama] object WireSpell extends NodeSpell[Wire] {
  def apply(in: Node, em: EnclosingModule) = new Wire(in.storage, em)
}

class Wire(storage: NodeStore, em: EnclosingModule) extends Connectable(storage, em) {
  createSelf()
}
object Wire {
  def apply[D<:Data](model: D)(implicit em: EnclosingModule): D = {
    model.copy.rebind(WireSpell)
  }
}

