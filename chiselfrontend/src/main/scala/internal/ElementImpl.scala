package gama
import internal._

case object ImproperElementRebindException extends ChiselException("Cannot change target of an Element after being bound to a non-SPEC node.")
abstract class ElementImpl(initialNode: Node) { // MUTABLE STATE: node
  self: Element =>

  private[this] var _node: Node = initialNode 
  // Elements DO NOT store an EnclosingModule... their nodes MAY (if Synthesizable)
  def node = _node
  protected[gama] def node_=(that: Node): Unit = {
    if(!node.isInstanceOf[SPEC]) {throw ImproperElementRebindException}
    _node = that
  }

  def nodes = Seq(node)

  protected[gama] def rebind(xform: NodeSpell[_<:Node]): this.type = {
    node = xform(node)
    this
  }

  def propogateName(newname: NameTree, newsource: NameSource): Unit = {}
  def propogateDescRef(newdesc: Desc): Unit = {NodeCheck.assertSynthesizable(this)}
}

trait ElementObjectImpl {
  trait ConnectSelfImpl[E<:Element] extends ConnectSelf.ConnectSelfImpl[E] {
    def verifyConnectSelf(sink: Sink[E], source: Source[E]): Unit = {}
  }
}


