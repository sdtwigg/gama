package gama
import internal._

object SInt {
  def apply(): SInt           = apply(None)
  def apply(width: Int): SInt = apply(Some(width))
  def apply(width: Option[Int]) = new SInt(new SPEC(SBits(width), None))

  implicit object basicfunctionality
    extends Muxable[SInt]
    with Element.ConnectToImpl[SInt,Digital]
    with Element.BiConnectImpl[SInt,SInt]
  {
    def muxRetVal(tc: SInt, fc: SInt): SInt = SInt()
  }
  implicit object withuintfunctionality
    extends Element.BiConnectImpl[SInt,UInt]
  // TODO: Is this actually ok? Not always clear what is going on....
}
final class SInt(initialNode: Node) extends Digital(initialNode) {
  // GENERAL ELEMENT REQUIREMENTS
  def :=(source: Digital)(implicit em: EnclosingModule): Unit =
    ConnectTo[SInt,Digital].monoConnect(Sink(this), Source(source), em)
  def <>(right: SInt)(implicit em: EnclosingModule): Unit =
    BiConnect[SInt,SInt].biConnect(Left(this), Right(right), em)
  def <>(right: UInt)(implicit em: EnclosingModule): Unit =
    BiConnect[SInt,UInt].biConnect(Left(this), Right(right), em)
  def copy = new SInt(new SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
  
  // IMPLEMENT EQUALITY AND OTHER COMPARISONS
  def ===(that: SInt)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpEqual, (this, that), em)
  def !==(that: SInt)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpNotEq, (this, that), em)
  
  def   <(that: SInt)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpLess,  (this, that), em)
  def  <=(that: SInt)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpLeEq,  (this, that), em)
  def   >(that: SInt)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpGrt,   (this, that), em)
  def  >=(that: SInt)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpGrEq,  (this, that), em)
  
  def   &(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpAnd, (this, that), em)
  def   |(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpOr,  (this, that), em)
  def   ^(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpXor, (this, that), em)
  
  // IMPLEMENT SIMPLE ABSTRACT OPERATIONS
  def andR(implicit em: EnclosingModule): Bool = this === api.S(-1)
  def  orR(implicit em: EnclosingModule): Bool = this !== api.S(0)
  
  def pad(that: Digital)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpPadTo,  (this, that), em)
  def  <<(that: UInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpLShft, (this, that), em)
  def  >>(that: UInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpRShft, (this, that), em)
  
  def toUInt(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpToUInt, this, None, em)
  def toSInt(implicit em: EnclosingModule): SInt = UnaryOp.SInt(OpIDENT,  this, None, em)
  
  def asUInt(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpAsUInt, this, getWidth, em)
  def asSInt(implicit em: EnclosingModule): SInt = UnaryOp.SInt(OpIDENT,  this, getWidth, em)
  
  def unary_~(implicit em: EnclosingModule): SInt = UnaryOp.SInt(OpNot,  this, this.getWidth, em)
  def unary_-(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpSubt, (api.S(0),this), em)

  // IMPLEMENT OPERATIONS WITH SELF
  def +(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpPlus, (this, that), em)
  def -(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpSubt, (this, that), em)
  def *(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpMult, (this, that), em)
  def /(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpDiv,  (this, that), em)
  def %(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpMod,  (this, that), em)

  // IMPLEMENT OPERATIONS WITH OTHERS
  def +(that: UIntLike)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpPlus, (this, that.toSInt), em)
  def -(that: UIntLike)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpSubt, (this, that.toSInt), em)
  def *(that: UIntLike)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpMult, (this, that.toSInt), em)
  def /(that: UIntLike)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpDiv,  (this, that.toSInt), em)
  def %(that: UIntLike)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpMod,  (this, that.toSInt), em)
}
