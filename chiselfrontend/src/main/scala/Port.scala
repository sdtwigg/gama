package gama
import internal._

protected[gama] object PortSpell extends NodeSpell[Port] {
  def apply(in: Node, em: EnclosingModule) = new Port(in.storage, em)
}

class Port(storage: NodeStore, em: EnclosingModule) extends Connectable(storage, em)
object Port {
  protected[gama] def apply[D<: Data](model: D)(implicit em: EnclosingModule): D = {
    model.copy.rebind(PortSpell)
  }
}
