package gama
import internal._

object UInt {
  def apply(): UInt           = apply(None)
  def apply(width: Int): UInt = apply(Some(width))
  def apply(width: Option[Int]) = new UInt(new SPEC(UBits(width), None))

  implicit object basicfunctionality extends Element.SelfMuxableImpl[UInt] with Element.ConnectSelfImpl[UInt] 
}
// Placing object first lets the class find the implicits in the object
class UInt(initialNode: Node) extends Bits(initialNode) {
  def :=(source: UInt)(implicit em: EnclosingModule) = ConnectSelf[UInt].connectSelf(source, this, em)
  def copy = new UInt(new SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]

  def +(that: UInt)(implicit em: EnclosingModule): UInt    = BinaryOp.UInt(OpPlus, (this, that), em)
  def -(that: UInt)(implicit em: EnclosingModule): UInt    = BinaryOp.UInt(OpSubt, (this, that), em)
  def *(that: UInt)(implicit em: EnclosingModule): UInt    = BinaryOp.UInt(OpMult, (this, that), em)
  def /(that: UInt)(implicit em: EnclosingModule): UInt    = BinaryOp.UInt(OpDiv,  (this, that), em)
  def %(that: UInt)(implicit em: EnclosingModule): UInt    = BinaryOp.UInt(OpMod,  (this, that), em)

  def pad(that: Bits)(implicit em: EnclosingModule): UInt  = BinaryOp.UInt(OpPadTo, (this, that), em)
  def  ##(that: Bits)(implicit em: EnclosingModule): UInt  = BinaryOp.UInt(OpCat,   (this, that), em)
}
