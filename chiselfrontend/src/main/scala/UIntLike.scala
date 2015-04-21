package gama
import internal._

object UIntLike {
  implicit object basicfunctionality extends Muxable[UIntLike] {
    def muxRetVal(tc: UIntLike, fc: UIntLike) = UInt()
  }
}
abstract class UIntLike(initialNode: Node) extends Digital(initialNode) {
  protected type Self <: UIntLike
  protected type MultiSelf = UInt // Can hold multiple bits

  // external -> internal API
  // New operations
  def do_eq (that: UIntLike, em: EnclosingModule): Bool = BinaryOp.Bool(OpEqual, (this, that), em)
  def do_neq(that: UIntLike, em: EnclosingModule): Bool = BinaryOp.Bool(OpNotEq, (this, that), em)
  
  def do_lt (that: UIntLike, em: EnclosingModule): Bool = BinaryOp.Bool(OpLess,  (this, that), em)
  def do_lte(that: UIntLike, em: EnclosingModule): Bool = BinaryOp.Bool(OpLeEq,  (this, that), em)
  def do_gt (that: UIntLike, em: EnclosingModule): Bool = BinaryOp.Bool(OpGrt,   (this, that), em)
  def do_gtw(that: UIntLike, em: EnclosingModule): Bool = BinaryOp.Bool(OpGrEq,  (this, that), em)
  
  def do_and(that: UIntLike, em: EnclosingModule): UInt = BinaryOp.UInt(OpAnd, (this, that), em)
  def do_or (that: UIntLike, em: EnclosingModule): UInt = BinaryOp.UInt(OpOr,  (this, that), em)
  def do_xor(that: UIntLike, em: EnclosingModule): UInt = BinaryOp.UInt(OpXor, (this, that), em)
  
  // Implementations for parent abstracts
  def do_andR(em: EnclosingModule): Bool = this.do_eq( LiteralUInt(0) ,em).do_not(em)
  def do_orR (em: EnclosingModule): Bool = this.do_neq(LiteralUInt(0), em)
  
  def do_neg(em: EnclosingModule): MultiSelf = BinaryOp.UInt(OpSubt, (api.U(0),this), em) 
  
  def do_toUInt(em: EnclosingModule): UInt = UnaryOp.UInt(OpIDENT,  this, None, em)
  def do_toSInt(em: EnclosingModule): SInt = UnaryOp.SInt(OpToSInt, this, None, em)
  def do_asUInt(em: EnclosingModule): UInt = UnaryOp.UInt(OpIDENT,  this, getWidth, em)
  def do_asSInt(em: EnclosingModule): SInt = UnaryOp.SInt(OpAsSInt, this, getWidth, em)
  
  // BINARY OPERATIONS
  def do_lshft(that: UInt, em: EnclosingModule): MultiSelf = BinaryOp.UInt(OpLShft, (this, that), em)
  def do_rshft(that: UInt, em: EnclosingModule): MultiSelf = BinaryOp.UInt(OpRShft, (this, that), em)
  
  def do_add(that: UIntLike, em: EnclosingModule): MultiSelf = BinaryOp.UInt(OpPlus, (this, that), em)
  def do_sub(that: UIntLike, em: EnclosingModule): MultiSelf = BinaryOp.UInt(OpSubt, (this, that), em)
  def do_mul(that: UIntLike, em: EnclosingModule): MultiSelf = BinaryOp.UInt(OpMult, (this, that), em)
  def do_div(that: UIntLike, em: EnclosingModule): MultiSelf = BinaryOp.UInt(OpDiv,  (this, that), em)
  def do_mod(that: UIntLike, em: EnclosingModule): MultiSelf = BinaryOp.UInt(OpMod,  (this, that), em)

  def do_add(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpPlus, (this.do_toSInt(em), that), em)
  def do_sub(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpSubt, (this.do_toSInt(em), that), em)
  def do_mul(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpMult, (this.do_toSInt(em), that), em)
  def do_div(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpDiv,  (this.do_toSInt(em), that), em)
  def do_mod(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpMod,  (this.do_toSInt(em), that), em)

  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}
  // External API
  def ===(that: UIntLike): Bool = macro XFORM.do_eq.thatarg
  def !==(that: UIntLike): Bool = macro XFORM.do_neq.thatarg
  
  def   <(that: UIntLike): Bool = macro XFORM.do_lt.thatarg
  def  <=(that: UIntLike): Bool = macro XFORM.do_lte.thatarg
  def   >(that: UIntLike): Bool = macro XFORM.do_gt.thatarg
  def  >=(that: UIntLike): Bool = macro XFORM.do_gte.thatarg
  
  def   &(that: UIntLike): UInt = macro XFORM.do_and.thatarg
  def   |(that: UIntLike): UInt = macro XFORM.do_or.thatarg
  def   ^(that: UIntLike): UInt = macro XFORM.do_xor.thatarg
}
