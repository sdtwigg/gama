package gama
import internal._

trait NodeSpell[Out<:Synthesizable] {
  def apply(in: Node, em: EnclosingModule): Out
}

abstract class Data {
  def copy: this.type
  protected[gama] def rebind(xform: NodeSpell[_<:Synthesizable])(implicit em: EnclosingModule): this.type
}

trait SelfTransfer[D<:Data] {
  def selfTransfer(source: D, sink: D)(implicit em: EnclosingModule): D
}

