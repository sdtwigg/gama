package gama
import internal._

object UInt {
  def apply(): UInt           = apply(None)
  def apply(width: Int): UInt = apply(Some(width))
  def apply(width: Option[Int]) = new UInt(new SPEC(UBits(width), None))
  
  implicit object basicfunctionality
    extends Muxable[UInt]
    with Element.ConnectToImpl[UInt,Digital]
    with Element.BiConnectImpl[UInt,UInt]
  {
    def muxRetVal(tc: UInt, fc: UInt): UInt = UInt()
  }
  implicit object withsintfunctionality
    extends Element.BiConnectImpl[UInt,SInt]
  // TODO: Is this actually ok? Not always clear what is going on....
}
// Placing object first lets the class find the implicits in the object
final class UInt(initialNode: Node) extends UIntLike(initialNode) {
  protected type Self = UInt

  def :=(source: Digital)(implicit em: EnclosingModule): Unit =
    ConnectTo[UInt,Digital].monoConnect(Sink(this), Source(source), em)
  def <>(right: UInt)(implicit em: EnclosingModule): Unit =
    BiConnect[UInt,UInt].biConnect(Left(this), Right(right), em)
  def <>(right: SInt)(implicit em: EnclosingModule): Unit =
    BiConnect[UInt,SInt].biConnect(Left(this), Right(right), em)
  def copy = new UInt(new SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
  
  // implementation for parent abstracts
  def do_not(info: EnclosureInfo): Self = UnaryOp.UInt(OpNot, this, this.getWidth, info)
}
