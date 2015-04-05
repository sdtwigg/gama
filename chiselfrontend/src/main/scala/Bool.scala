package gama
import internal._

object Bool {
  def apply() = new Bool(SPEC(UBits(Some(1)), None))

  implicit object basicfunctionality extends Element.SelfMuxableImpl[Bool] with Element.ConnectSelfImpl[Bool] 
  import scala.language.implicitConversions
  implicit def bool2UInt(in: Bool)(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpToUInt, in, Some(1), em) 
}
class Bool(initialNode: Node) extends Bits(initialNode) {
  def copy = new Bool(SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
  def :=(source: Bool)(implicit em: EnclosingModule) = ConnectSelf[Bool].connectSelf(source, this, em)

  def unary_!(implicit em: EnclosingModule): Bool = UnaryOp.Bool(OpNot, this, em)

  def &&(that: Bool)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpAnd, (this, that), em)
  def ||(that: Bool)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpOr,  (this, that), em)
  def  ^(that: Bool)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpXor, (this, that), em)
}
