package gama

object RegSpell extends NodeSpell[Reg] {
  def apply[NS<:NodeStore](in: Node[NS]) = new Reg(in.storage)
}

class Reg[+NS <: NodeStore](storage: NS) extends Node(storage)
object Reg {
  def makeReg[D[X<:Node[Y],Y<:NS]<:Data[X,Y],NS<:NodeStore](model: D[_<:Node[NS],NS])(implicit reagent: Regenerate[D,NS]) = {
    reagent.regenerate(model, RegSpell)
  }
}
