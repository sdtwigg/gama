package gama
import internal._

object UInt {
  def apply(): UInt           = apply(None)
  def apply(width: Int): UInt = apply(Some(width))
  def apply(width: Option[Int]) = new UInt(new SPEC(UBits(width)))

  implicit object basicfunctionality extends SelfMuxable[UInt] with SelfTransfer[UInt] {
    def mux(cond: Bool, tc: UInt, fc: UInt)(implicit em: EnclosingModule) = new UInt(new Mux(UBits(None), cond, tc, fc))
    def selfTransfer(source: UInt, sink: UInt)(implicit em: EnclosingModule) = Element.genSelfTransferImpl(source, sink)(em)
  }
}
// Placing object first lets the class find the implicits in the object
class UInt(initialNode: Node) extends Bits(initialNode) {
  def :=(source: UInt)(implicit em: EnclosingModule) = implicitly[SelfTransfer[UInt]].selfTransfer(source, this)
  def copy = new UInt(new SPEC(node.storage)).asInstanceOf[this.type]

  def +(that: UInt)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpPlus, (this.node, that.node), em)
}
