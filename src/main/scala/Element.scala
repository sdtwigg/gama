package gama

abstract class Element(val node: Node) extends Data

abstract class Bits(node: Node) extends Element(node)
/*
// Making this available would allow muxing between UInt and SInt (and thus making a vector of them)
object Bits {
  implicit object selfmuxer extends SelfMuxable[Bits] {
    def mux(tc: Bits, fc: Bits) = ???
  }
}
*/

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
