package gama

abstract class Element(val node: Node) extends Data

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
    def regenerate(in: UInt, xform: NodeSpell[_<:Node]) = new UInt(xform(in.node))
    def mux(tc: UInt, fc: UInt) = new UInt(new Mux(RawUBits(None)))
    def selfTransfer(source: UInt, sink: UInt) = sink
  }
}
// Placing object first lets the class find the implicits in the object
class UInt private (node: Node) extends Bits(node) {
  def :=(source: UInt) = implicitly[SelfTransfer[UInt]].selfTransfer(source, this)
}

object Bool {
  def apply() = new Bool(new SPEC(RawUBits(Some(1))))

  implicit object basicfunctionality extends Regenerate[Bool] with SelfMuxable[Bool] with SelfTransfer[Bool] {
    def regenerate(in: Bool, xform: NodeSpell[_<:Node]) = new Bool(xform(in.node))
    def mux(tc: Bool, fc: Bool) = new Bool(new Mux(RawUBits(Some(1))))
    def selfTransfer(source: Bool, sink: Bool) = sink
  }

  import scala.language.implicitConversions
  implicit def bool2UInt(in: Bool): UInt = UInt(1) // TODO IMPLEMENT
}
class Bool private (node: Node) extends Bits(node) {
  def :=(source: Bool) = implicitly[SelfTransfer[Bool]].selfTransfer(source, this)
}

object SInt {
  def apply(): SInt           = apply(None)
  def apply(width: Int): SInt = apply(Some(width))
  def apply(width: Option[Int]) = new SInt(new SPEC(RawSBits(width)))

  implicit object basicfunctionality extends Regenerate[SInt] with SelfMuxable[SInt] with SelfTransfer[SInt] {
    def regenerate(in: SInt, xform: NodeSpell[_<:Node]) = new SInt(xform(in.node))
    def mux(tc: SInt, fc: SInt) = new SInt(new Mux(RawSBits(None)))
    def selfTransfer(source: SInt, sink: SInt) = sink
  }
}
class SInt private (node: Node) extends Bits(node) {
  def :=(source: SInt) = implicitly[SelfTransfer[SInt]].selfTransfer(source, this)
}
