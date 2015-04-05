package gama
import internal._
/*
  General contract for copy is that it creates another Data of the same fully derived type (like clone).
  Regarding potential mutable state in Data (and subtypes):
  -> node: all nodes should be connected to a SPEC node, with the same NodeStorage as before
  -> name: should not be copied (and thus reset to None)
  -> descRef: should not be copied (and thus reset to None)
*/

abstract class Data extends Nameable with DescReference {
  def copy: this.type
  protected[gama] def rebind(xform: NodeSpell[_<:Node]): this.type

  def nodes: Seq[Node]
}

@annotation.implicitNotFound("Cannot connect elements of type ${D}. No implicit ConnectSelf[${D}] available")
trait ConnectSelf[D<:Data] {
  def connectSelf(source: D, sink: D, em: EnclosingModule): D
}
object ConnectSelf {
  def apply[D<:Data: ConnectSelf] = implicitly[ConnectSelf[D]]
  trait ConnectSelfImpl[D<:Data] extends ConnectSelf[D] {
    def verifyConnectSelf(source: D, sink: D): Unit
    def connectSelf(source: D, sink: D, em: EnclosingModule): D = {
      em.getActiveJournal.append(ConnectData(source, sink))
      sink
    }
  }
}
