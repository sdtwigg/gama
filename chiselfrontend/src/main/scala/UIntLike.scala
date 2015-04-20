package gama
import internal._

object UIntLike {
  implicit object basicfunctionality extends Muxable[UIntLike] {
    def muxRetVal(tc: UIntLike, fc: UIntLike) = UInt()
  }
}
abstract class UIntLike(initialNode: Node) extends Digital(initialNode) {
  // external -> internal API
  def do_andR(em: EnclosingModule): Bool = this.do_eq( LiteralUInt(0) ,em).do_not(em)
  def do_orR (em: EnclosingModule): Bool = this.do_neq(LiteralUInt(0), em)
  
  def do_not(em: EnclosingModule): UIntLike
  
  def do_eq (that: UIntLike, em: EnclosingModule): Bool = BinaryOp.Bool(OpEqual, (this, that), em)
  def do_neq(that: UIntLike, em: EnclosingModule): Bool = BinaryOp.Bool(OpNotEq, (this, that), em)

  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}
  // External API
  def ===(that: UIntLike): Bool = macro XFORM.do_eq.thatarg
  def !==(that: UIntLike): Bool = macro XFORM.do_neq.thatarg
  
  override def unary_~(): UIntLike = macro XFORM.do_not.paren

  // TO BE CONVERTED VVVV
  // IMPLEMENT BITWISE OPs, inc. EQUALITY AND OTHER COMPARISONS
  
  def   <(that: UIntLike)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpLess,  (this, that), em)
  def  <=(that: UIntLike)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpLeEq,  (this, that), em)
  def   >(that: UIntLike)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpGrt,   (this, that), em)
  def  >=(that: UIntLike)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpGrEq,  (this, that), em)
  
  def   &(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpAnd, (this, that), em)
  def   |(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpOr,  (this, that), em)
  def   ^(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpXor, (this, that), em)

  // IMPLEMENT SIMPLE ABSTRACT OPERATIONS
  def  <<(that: UInt)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpLShft, (this, that), em)
  def  >>(that: UInt)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpRShft, (this, that), em)

  def toUInt(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpIDENT,  this, None, em)
  def toSInt(implicit em: EnclosingModule): SInt = UnaryOp.SInt(OpToSInt, this, None, em)
  
  def asUInt(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpIDENT,  this, getWidth, em)
  def asSInt(implicit em: EnclosingModule): SInt = UnaryOp.SInt(OpAsSInt, this, getWidth, em)
  
  def unary_-(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpSubt, (api.U(0),this), em)

  // IMPLEMENT OPERATIONS WITH SELF
  def +(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpPlus, (this, that), em)
  def -(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpSubt, (this, that), em)
  def *(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpMult, (this, that), em)
  def /(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpDiv,  (this, that), em)
  def %(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpMod,  (this, that), em)

  // IMPLEMENT OPERATIONS WITH OTHERS
  def +(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpPlus, (this.toSInt, that), em)
  def -(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpSubt, (this.toSInt, that), em)
  def *(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpMult, (this.toSInt, that), em)
  def /(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpDiv,  (this.toSInt, that), em)
  def %(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpMod,  (this.toSInt, that), em)
}
