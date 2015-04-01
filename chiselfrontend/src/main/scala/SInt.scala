package gama
import internal._

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