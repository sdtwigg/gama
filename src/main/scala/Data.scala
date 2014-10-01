package gama

class Data //(implicit val owner: EnclosingModule)

trait Regenerate[D<:Data] {
  def regenerate(in: D, xform: NodeSpell[_<:Node]): D
}

trait SelfTransfer[D<:Data] {
  def selfTransfer(source: D, sink: D): D
}

trait NodeSpell[Out<:Node] {
  def apply(in: Node): Out
}

