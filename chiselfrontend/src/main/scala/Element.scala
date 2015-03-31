package gama
import internal._

case object ImproperElementRebindException extends ChiselException("Cannot change target of an Element after being bound to a non-SPEC node.")
abstract class Element(private[this] var _node: Node) extends Data {
  def node = _node
  protected[gama] def node_=(that: Synthesizable): Unit = {
    if(!node.isInstanceOf[SPEC]) {throw ImproperElementRebindException}
    _node = that
  }

  def nodes = Seq(node)

  protected[gama] def rebind(xform: NodeSpell[_<:Synthesizable])(implicit em: EnclosingModule): this.type = {
    node = xform(node, em)
    this
  }

  override def toString = s"${getClass.getSimpleName}(${node})"
}
case object UnenclosedTransferException extends ChiselException("Data transfer cannot occur outside an EnclosingModule")
object Element {
  def genSelfTransferImpl[E<:Element](source: E, sink: E)(implicit em: EnclosingModule) = {
    // TODO: CONSIDER: CHECK FOR CROSSMODULE MIS-ASSIGNMENTS?
    em.getOrElse(
      throw UnenclosedTransferException
    ).getActiveJournal.append(NodeAssign(source.node, sink.node))

    sink
  }
}

abstract class Bits(node: Node) extends Element(node)
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

object UInt {
  def apply(): UInt           = apply(None)
  def apply(width: Int): UInt = apply(Some(width))
  def apply(width: Option[Int]) = new UInt(new SPEC(RawUBits(width)))

  implicit object basicfunctionality extends SelfMuxable[UInt] with SelfTransfer[UInt] {
    def mux(cond: Bool, tc: UInt, fc: UInt)(implicit em: EnclosingModule) = new UInt(new Mux(RawUBits(None), cond, tc, fc))
    def selfTransfer(source: UInt, sink: UInt)(implicit em: EnclosingModule) = Element.genSelfTransferImpl(source, sink)(em)
  }
}
// Placing object first lets the class find the implicits in the object
class UInt(_node: Node) extends Bits(_node) {
  def :=(source: UInt)(implicit em: EnclosingModule) = implicitly[SelfTransfer[UInt]].selfTransfer(source, this)
  def copy = new UInt(new SPEC(node.storage)).asInstanceOf[this.type]
}

object Bool {
  def apply() = new Bool(new SPEC(RawUBits(Some(1))))

  implicit object basicfunctionality extends SelfMuxable[Bool] with SelfTransfer[Bool] {
    def mux(cond: Bool, tc: Bool, fc: Bool)(implicit em: EnclosingModule) = new Bool(new Mux(RawUBits(Some(1)), cond, tc, fc))
    def selfTransfer(source: Bool, sink: Bool)(implicit em: EnclosingModule) = Element.genSelfTransferImpl(source, sink)(em)
  }

  import scala.language.implicitConversions
  implicit def bool2UInt(in: Bool)(implicit em: EnclosingModule): UInt = new UInt(new Op(RawUBits(Some(1)), em)) // TODO IMPLEMENT
}
class Bool(node: Node) extends Bits(node) {
  def :=(source: Bool)(implicit em: EnclosingModule) = implicitly[SelfTransfer[Bool]].selfTransfer(source, this)
  def copy = new Bool(new SPEC(node.storage)).asInstanceOf[this.type]
}

object SInt {
  def apply(): SInt           = apply(None)
  def apply(width: Int): SInt = apply(Some(width))
  def apply(width: Option[Int]) = new SInt(new SPEC(RawSBits(width)))

  implicit object basicfunctionality extends SelfMuxable[SInt] with SelfTransfer[SInt] {
    def mux(cond: Bool, tc: SInt, fc: SInt)(implicit em: EnclosingModule) = new SInt(new Mux(RawSBits(None), cond, tc, fc))
    def selfTransfer(source: SInt, sink: SInt)(implicit em: EnclosingModule) = Element.genSelfTransferImpl(source, sink)(em)
  }
}
class SInt(node: Node) extends Bits(node) {
  def :=(source: SInt)(implicit em: EnclosingModule) = implicitly[SelfTransfer[SInt]].selfTransfer(source, this)
  def copy = new SInt(new SPEC(node.storage)).asInstanceOf[this.type]
}
