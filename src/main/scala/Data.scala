package gama
import internal._

trait NodeSpell[Out<:Node] {
  def apply(in: Node, em: EnclosingModule): Out
}

class Data

trait Regenerate[D<:Data] {
  def regenerate(in: D, xform: NodeSpell[_<:Node])(implicit em: EnclosingModule): D
}

trait SelfTransfer[D<:Data] {
  def selfTransfer(source: D, sink: D)(implicit em: EnclosingModule): D
}

