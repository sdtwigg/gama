package gama
import internal._

object Bool {
  def apply() = new Bool(SPEC(UBits(Some(1)), None))

  implicit object basicfunctionality extends Element.SelfMuxableImpl[Bool] with Element.SelfTransferImpl[Bool] 
  import scala.language.implicitConversions
  implicit def bool2UInt(in: Bool)(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpToUInt, in, Some(1), em) 
}
class Bool(initialNode: Node) extends Bits(initialNode) {
  def :=(source: Bool)(implicit em: EnclosingModule) = implicitly[SelfTransfer[Bool]].selfTransfer(source, this, em)
  def copy = new Bool(SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
  
}
