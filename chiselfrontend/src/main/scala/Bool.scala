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
  protected type Self = Bool

  def :=(source: Bool)(implicit em: EnclosingModule): Unit =
    ConnectTo[Bool,Bool].monoConnect(Sink(this), Source(source), em)
  def <>(right: Bool)(implicit em: EnclosingModule): Unit =
    BiConnect[Bool,Bool].biConnect(Left(this), Right(right), em)
  def copy = new Bool(SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
  
  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}
  // External API
  def unary_!(): Bool = macro XFORM.do_not.paren

  def &&(that: Bool): Bool = macro XFORM.do_andB.thatarg
  def ||(that: Bool): Bool = macro XFORM.do_orB.thatarg
  def ^^(that: Bool): Bool = macro XFORM.do_xorB.thatarg
  
  // external->internal API
  def do_not(info: EnclosureInfo): Self = UnaryOp.Bool(OpNot, this, info)
  
  def do_andB(that: Bool, info: EnclosureInfo): Bool = BinaryOp.Bool(OpAnd, (this, that), info)
  def do_orB (that: Bool, info: EnclosureInfo): Bool = BinaryOp.Bool(OpOr,  (this, that), info)
  def do_xorB(that: Bool, info: EnclosureInfo): Bool = BinaryOp.Bool(OpXor, (this, that), info)
}
