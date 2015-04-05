package gama
import internal._

object SInt {
  def apply(): SInt           = apply(None)
  def apply(width: Int): SInt = apply(Some(width))
  def apply(width: Option[Int]) = new SInt(new SPEC(SBits(width), None))

  implicit object basicfunctionality extends Element.SelfMuxableImpl[SInt] with Element.ConnectSelfImpl[SInt]
}
class SInt(initialNode: Node) extends Bits(initialNode) {
  def :=(source: SInt)(implicit em: EnclosingModule) = ConnectSelf[SInt].connectSelf(source, this, em)
  def copy = new SInt(new SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
  
  def pad(that: Bits)(implicit em: EnclosingModule): SInt  = BinaryOp.SInt(OpPad,  (this, that), em)
}
