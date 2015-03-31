package gama
import internal._

abstract class Data {
  def copy: this.type
  protected[gama] def rebind(xform: NodeSpell[_<:Synthesizable])(implicit em: EnclosingModule): this.type

  def nodes: Seq[Node]
}

trait SelfTransfer[D<:Data] {
  def selfTransfer(source: D, sink: D)(implicit em: EnclosingModule): D
}

