package gama
import internal._

object Bool {
  def apply() = new Bool(SPEC(UBits(Some(1)), None))
  
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
  
  // external->internal API
  def do_not(em: EnclosingModule): Bool = UnaryOp.Bool(OpNot, this, em)

  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}
  // External API
  override def unary_~(): Bool = macro XFORM.do_not.paren
           def unary_!(): Bool = macro XFORM.do_not.paren

  def &&(that: Bool)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpAnd, (this, that), em)
  def ||(that: Bool)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpOr,  (this, that), em)
  def ^^(that: Bool)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpXor, (this, that), em)
}
