package gama
import internal._

object WireSpell extends NodeSpell[Wire] {
  def apply(in: Node, em: EnclosingModule) = new Wire(in.storage, em)
}

class Wire(storage: NodeStore, em: EnclosingModule) extends Synthesizable(storage, em)
object Wire {
  def apply[D<:Data : Regenerate](model: D)(implicit em: EnclosingModule): D = {
    implicitly[Regenerate[D]].regenerate(model, WireSpell)
  }
}

