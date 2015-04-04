package gama
import internal._
/*
  General contract for copy is that it creates another Data of the same fully derived type (like clone).
  Regarding potential mutable state in Data (and subtypes):
  -> node: all nodes should be connected to a SPEC node, with the same NodeStorage as before
  -> name: should not be copied (and thus reset to None)
  -> opRef: should not be copied (and thus reset to None)
*/

abstract class Data extends Nameable with DescReference {
  def copy: this.type
  protected[gama] def rebind(xform: NodeSpell[_<:Synthesizable], em: EnclosingModule): this.type

  def nodes: Seq[Node]
}

trait SelfTransfer[D<:Data] {
  def selfTransfer(source: D, sink: D, em: EnclosingModule): D
}
object SelfTransfer {
  trait SelfTransferImpl[D<:Data] extends SelfTransfer[D] {
    def verifyTransfer(source: D, sink: D): Unit
    def selfTransfer(source: D, sink: D, em: EnclosingModule): D = {
      em.getActiveJournal.append(DataTransfer(source, sink))
      sink
    }
  }
}
