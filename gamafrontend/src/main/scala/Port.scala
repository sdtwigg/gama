package gama
import internal._

object PortSpell extends NodeSpell[Port] {
  def apply(in: Node, em: EnclosingModule) = new Port(in.storage, em)
}

class Port(storage: NodeStore, em: EnclosingModule) extends Connectable(storage, em)
object Port {
  protected[gama] def apply[D<: Data : Regenerate](model: D)(implicit em: EnclosingModule): D = {
    implicitly[Regenerate[D]].regenerate(model, PortSpell)
  }
}
