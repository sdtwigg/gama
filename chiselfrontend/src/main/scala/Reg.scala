package gama
import internal._

protected[gama] object RegSpell extends NodeSpell[Reg] {
  def apply(in: Node, em: EnclosingModule) = new Reg(in.storage, em)
}

class Reg(storage: NodeStore, em: EnclosingModule) extends Synthesizable(storage, em) {
  createSelf()
}
object Reg {
  def apply[D<:Data](model: D)(implicit em: EnclosingModule): D = {
    model.copy.rebind(RegSpell)
  }
}
