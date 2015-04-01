package gama
import internal._

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
