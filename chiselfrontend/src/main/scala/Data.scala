package gama
import internal._
/*
  General contract for copy is that it creates another Data of the same fully derived type (like clone).
  Regarding potential mutable state in Data (and subtypes):
  -> node: all nodes should be connected to a SPEC node, with the same NodeStorage as before
  -> name: should not be copied (and thus reset to None)
*/

abstract class Data extends Nameable {
  def copy: this.type
  protected[gama] def rebind(xform: NodeSpell[_<:Synthesizable])(implicit em: EnclosingModule): this.type

  def nodes: Seq[Node]
}

trait SelfTransfer[D<:Data] {
  def selfTransfer(source: D, sink: D)(implicit em: EnclosingModule): D
}

