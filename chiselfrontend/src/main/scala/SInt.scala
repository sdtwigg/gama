package gama
import internal._

object SInt {
  def apply(): SInt           = apply(None)
  def apply(width: Int): SInt = apply(Some(width))
  def apply(width: Option[Int]) = new SInt(new SPEC(SBits(width)))

  implicit object basicfunctionality extends Element.SelfMuxableImpl[SInt] with Element.SelfTransferImpl[SInt]
}
class SInt(initialNode: Node) extends Bits(initialNode) {
  def :=(source: SInt)(implicit em: EnclosingModule) = implicitly[SelfTransfer[SInt]].selfTransfer(source, this, em)
  def copy = new SInt(new SPEC(node.storage)).asInstanceOf[this.type]
}
