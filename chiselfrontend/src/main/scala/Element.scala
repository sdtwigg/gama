package gama
import internal._

case object ImproperElementRebindException extends ChiselException("Cannot change target of an Element after being bound to a non-SPEC node.")
abstract class Element(initialNode: Node) extends Data { // MUTABLE STATE: node
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

  def propogateName(): Unit = {}
  def propogateDescRef(): Unit = {NodeCheck.assertSynthesizable(this)}
}
object Element {
  trait SelfTransferImpl[E<:Element] extends SelfTransfer.SelfTransferImpl[E] {
    def verifyTransfer(source: E, sink: E): Unit = {}
  }
  trait SelfMuxableImpl[E<:Element] extends SelfMuxable[E] {
    def muxRetVal(tc: E, fc: E): E = tc.copy
  }
}

abstract class Bits(initialNode: Node) extends Element(initialNode) {
  def extract(position: Int)(implicit em: EnclosingModule): Bool = ExtractOp.Bool(this, position, em)
  def extract(left_pos: Int, right_pos: Int)(implicit em: EnclosingModule): UInt = ExtractOp.UInt(this, left_pos, right_pos, em)
  def apply(position: Int)(implicit em: EnclosingModule): Bool = extract(position)(em)
  def apply(left_pos: Int, right_pos: Int)(implicit em: EnclosingModule): UInt = extract(left_pos, right_pos)(em)
  
}
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

