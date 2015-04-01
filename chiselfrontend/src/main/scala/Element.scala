package gama
import internal._

case object ImproperElementRebindException extends ChiselException("Cannot change target of an Element after being bound to a non-SPEC node.")
abstract class Element(initialNode: Node) extends Data {
  private[this] var _node: Node = initialNode 
  // Elements DO NOT store an EnclosingModule... their nodes MAY (if Synthesizable)
  def node = _node
  protected[gama] def node_=(that: Node): Unit = {
    if(!node.isInstanceOf[SPEC]) {throw ImproperElementRebindException}
    _node = that
  }

  def nodes = Seq(node)

  protected[gama] def rebind(xform: NodeSpell[_<:Synthesizable])(implicit em: EnclosingModule): this.type = {
    node = xform(node, em)
    this
  }

  override def toString = s"${node}: ${Console.GREEN}${getClass.getSimpleName}${Console.RESET}|${node.storage}"
}

object Element {
  def genSelfTransferImpl[E<:Element](source: E, sink: E)(implicit em: EnclosingModule) = {
    // TODO: CONSIDER: CHECK FOR CROSSMODULE MIS-ASSIGNMENTS?
    em.getActiveJournal.append(NodeAssign(source, sink))

    sink
  }
}

abstract class Bits(initialNode: Node) extends Element(initialNode)
object Bits {
/*
// Making this available would allow muxing between UInt and SInt (and thus making a Vec of them)
  implicit object selfmuxer extends SelfMuxable[Bits] {
    def mux(tc: Bits, fc: Bits) = ???
  }
// These two allow muxing but not Vec creation
  implicit object usintmuxer1 extends Muxable[UInt, SInt] {
    def mux[D<:Data, A >: UInt <: D, B >: SInt <: D](tc: UInt, fc: SInt): D = ???.asInstanceOf[D]
  }
  implicit object usintmuxer2 extends Muxable[SInt, UInt] {
    def mux[D<:Data, A >: SInt <: D, B >: UInt <: D](tc: SInt, fc: IInt): D = ???.asInstanceOf[D]
  }
*/
}

