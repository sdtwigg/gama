package gama
import internal._

trait UIntApplies { // trait so Bits in api can mix this in as well
  def apply(): UInt           = apply(None)
  def apply(width: Int): UInt = apply(Some(width))
  def apply(width: Option[Int]) = new UInt(new SPEC(UBits(width), None))
}

object UInt extends UIntApplies {
  implicit object basicfunctionality extends Muxable[UInt] with Element.ConnectToImpl[UInt,Digital] {
    def muxRetVal(tc: UInt, fc: UInt): UInt = UInt()
  }
}
// Placing object first lets the class find the implicits in the object
final class UInt(initialNode: Node) extends UIntLike(initialNode) {
  def :=(source: Digital)(implicit em: EnclosingModule): Unit =
    ConnectTo[UInt,Digital].monoConnect(Sink(this), Source(source), em)
  def copy = new UInt(new SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
  
  def unary_~(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpNot, this, this.getWidth, em)
}
