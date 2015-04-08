package gama
import internal._

object UInt {
  def apply(): UInt           = apply(None)
  def apply(width: Int): UInt = apply(Some(width))
  def apply(width: Option[Int]) = new UInt(new SPEC(UBits(width), None))

  implicit object basicfunctionality extends SelfMuxable[UInt] with Element.ConnectSelfImpl[UInt] {
    def muxRetVal(tc: UInt, fc: UInt): UInt = UInt()
  }
}
// Placing object first lets the class find the implicits in the object
class UInt(initialNode: Node) extends UIntLike(initialNode) {
  def :=(source: UIntLike)(implicit em: EnclosingModule) = ConnectSelf[UIntLike].connectSelf(source, this, em)
  def copy = new UInt(new SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
}
