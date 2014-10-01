package gama

object RegSpell extends NodeSpell[Reg] {
  def apply(in: Node) = new Reg(in.storage)
}

class Reg(storage: NodeStore) extends Synthesizable(storage)
object Reg {
  def apply[D<:Data : Regenerate](model: D)(implicit em: EnclosingModule): D = {
    implicitly[Regenerate[D]].regenerate(model, RegSpell)
  }
}
