package gama
import internal._

trait BoolApplies {
  def apply() = new Bool(SPEC(UBits(Some(1)), None))
}

object Bool extends BoolApplies {
  implicit object basicfunctionality
    extends Muxable[Bool]
    with Element.ConnectToImpl[Bool,Bool]
    with Element.BiConnectImpl[Bool,Bool]
  {
    def muxRetVal(tc: Bool, fc: Bool): Bool = Bool()
  }
  //import scala.language.implicitConversions
  //implicit def bool2UInt(in: Bool)(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpToUInt, in, Some(1), em) 
}
final class Bool(initialNode: Node) extends UIntLike(initialNode) {
  def :=(source: Bool)(implicit em: EnclosingModule): Unit =
    ConnectTo[Bool,Bool].monoConnect(Sink(this), Source(source), em)
  def <>(right: Bool)(implicit em: EnclosingModule): Unit =
    BiConnect[Bool,Bool].biConnect(Left(this), Right(right), em)
  def copy = new Bool(SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]

  def unary_!(implicit em: EnclosingModule): Bool = UnaryOp.Bool(OpNot, this, em)
  def unary_~(implicit em: EnclosingModule): Bool = unary_!

  def &&(that: Bool)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpAnd, (this, that), em)
  def ||(that: Bool)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpOr,  (this, that), em)
  def ^^(that: Bool)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpXor, (this, that), em)
}
