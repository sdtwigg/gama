package gama

class Data

trait Regenerate[D<:Data] {
  def regenerate(in: D, xform: NodeSpell[_<:Node]): D
}

trait SelfTransfer[D<:Data] {
  def selfTransfer(source: D, sink: D): D
}

trait NodeSpell[Out<:Node] {
  def apply(in: Node): Out
}

