package gama

object WireSpell extends NodeSpell[Wire] {
  def apply(in: Node) = new Wire(in.storage)
}

class Wire(storage: NodeStore) extends Synthesizable(storage)
object Wire {
  def apply[D<:Data : Regenerate](model: D)(implicit em: EnclosingModule): D = {
    implicitly[Regenerate[D]].regenerate(model, WireSpell)
  }
}

