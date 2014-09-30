package gama

object WireSpell extends NodeSpell[Wire] {
  def apply(in: Node) = new Wire(in.storage)
}

class Wire(storage: NodeStore) extends Node(storage)
object Wire {
  def apply[D<:Data : Regenerate](model: D): D = {
    implicitly[Regenerate[D]].regenerate(model, WireSpell)
  }
}

