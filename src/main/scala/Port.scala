package gama

object PortSpell extends NodeSpell[Port] {
  def apply(in: Node) = new Port(in.storage)
}

class Port(storage: NodeStore) extends Wire(storage)
object Port {
  protected[gama] def apply[D<: Data : Regenerate](model: D)(implicit em: EnclosingModule): D = {
    implicitly[Regenerate[D]].regenerate(model, PortSpell)
  }
}
