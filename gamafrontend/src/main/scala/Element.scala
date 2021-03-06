package gama
import internal._

abstract class Element(val node: Node) extends Data
object Element {
  def genSelfTransferImpl[E<:Element](source: E, sink: E)(implicit em: EnclosingModule) = {
    // TODO: CONSIDER: CHECK FOR CROSSMODULE MIS-ASSIGNMENTS?
    em.getOrElse(
      throw new Exception("Data transfer between Nodes must occur inside an enclosing module")
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

  implicit object basicfunctionality extends Regenerate[UInt] with SelfMuxable[UInt] with SelfTransfer[UInt] {
    def regenerate(in: UInt, xform: NodeSpell[_<:Node])(implicit em: EnclosingModule) = new UInt(xform(in.node, em))
    def mux(cond: Bool, tc: UInt, fc: UInt)(implicit em: EnclosingModule) = new UInt(new Mux(RawUBits(None), cond, tc, fc))
    def selfTransfer(source: UInt, sink: UInt)(implicit em: EnclosingModule) = Element.genSelfTransferImpl(source, sink)(em)
  }
}
// Placing object first lets the class find the implicits in the object
class UInt(node: Node) extends Bits(node) {
  def :=(source: UInt)(implicit em: EnclosingModule) = implicitly[SelfTransfer[UInt]].selfTransfer(source, this)
}

object Bool {
  def apply() = new Bool(new SPEC(RawUBits(Some(1))))

  implicit object basicfunctionality extends Regenerate[Bool] with SelfMuxable[Bool] with SelfTransfer[Bool] {
    def regenerate(in: Bool, xform: NodeSpell[_<:Node])(implicit em: EnclosingModule) = new Bool(xform(in.node, em))
    def mux(cond: Bool, tc: Bool, fc: Bool)(implicit em: EnclosingModule) = new Bool(new Mux(RawUBits(Some(1)), cond, tc, fc))
    def selfTransfer(source: Bool, sink: Bool)(implicit em: EnclosingModule) = Element.genSelfTransferImpl(source, sink)(em)
  }

  import scala.language.implicitConversions
  implicit def bool2UInt(in: Bool)(implicit em: EnclosingModule): UInt = new UInt(new Op(RawUBits(Some(1)), em)) // TODO IMPLEMENT
}
class Bool(node: Node) extends Bits(node) {
  def :=(source: Bool)(implicit em: EnclosingModule) = implicitly[SelfTransfer[Bool]].selfTransfer(source, this)
}

object SInt {
  def apply(): SInt           = apply(None)
  def apply(width: Int): SInt = apply(Some(width))
  def apply(width: Option[Int]) = new SInt(new SPEC(RawSBits(width)))

  implicit object basicfunctionality extends Regenerate[SInt] with SelfMuxable[SInt] with SelfTransfer[SInt] {
    def regenerate(in: SInt, xform: NodeSpell[_<:Node])(implicit em: EnclosingModule) = new SInt(xform(in.node, em))
    def mux(cond: Bool, tc: SInt, fc: SInt)(implicit em: EnclosingModule) = new SInt(new Mux(RawSBits(None), cond, tc, fc))
    def selfTransfer(source: SInt, sink: SInt)(implicit em: EnclosingModule) = Element.genSelfTransferImpl(source, sink)(em)
  }
}
class SInt(node: Node) extends Bits(node) {
  def :=(source: SInt)(implicit em: EnclosingModule) = implicitly[SelfTransfer[SInt]].selfTransfer(source, this)
}
