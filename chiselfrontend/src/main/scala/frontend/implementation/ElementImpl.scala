package gama
package frontend
package implementation

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
  protected[gama] def mimic(model: Data, asSPEC: Boolean): Unit = {
    model match {
      case e: Element => {node =
        if(asSPEC) SPEC(e.node.storage, e.node.resolveDirection)
        else e.node
      }
      case _ => throw StructuralMimicException
    }
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
  trait ConnectToImpl[To<:Element,From<:Element] extends ConnectTo[To,From] {
    def monoDetails(sink: Sink[To], source: Source[From]): ConnectDetails = ConnectAll
  }
  trait BiConnectImpl[LT<:Element,RT<:Element] extends BiConnect[LT,RT] {
    def biDetails(left: Left[LT], right: Right[RT], info: EnclosureInfo): BiConnectDetails = 
      BiConnect.elemDetails(left.data.node, right.data.node, info)
  }
}

