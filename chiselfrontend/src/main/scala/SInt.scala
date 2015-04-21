package gama
import internal._

object SInt extends {
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
  protected type Self = SInt
  protected type MultiSelf = SInt // Can hold multiple bits

  // GENERAL ELEMENT REQUIREMENTS
  def :=(source: Digital)(implicit em: EnclosingModule): Unit =
    ConnectTo[SInt,Digital].monoConnect(Sink(this), Source(source), em)
  def <>(right: SInt)(implicit em: EnclosingModule): Unit =
    BiConnect[SInt,SInt].biConnect(Left(this), Right(right), em)
  def <>(right: UInt)(implicit em: EnclosingModule): Unit =
    BiConnect[SInt,UInt].biConnect(Left(this), Right(right), em)
  def copy = new SInt(new SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
  
  // external->internal API
  // New operations
  def do_eq (that: SInt, em: EnclosingModule): Bool = BinaryOp.Bool(OpEqual, (this, that), em)
  def do_neq(that: SInt, em: EnclosingModule): Bool = BinaryOp.Bool(OpNotEq, (this, that), em)
  
  def do_lt (that: SInt, em: EnclosingModule): Bool = BinaryOp.Bool(OpLess,  (this, that), em)
  def do_lte(that: SInt, em: EnclosingModule): Bool = BinaryOp.Bool(OpLeEq,  (this, that), em)
  def do_gt (that: SInt, em: EnclosingModule): Bool = BinaryOp.Bool(OpGrt,   (this, that), em)
  def do_gtw(that: SInt, em: EnclosingModule): Bool = BinaryOp.Bool(OpGrEq,  (this, that), em)
  
  def do_and(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpAnd, (this, that), em)
  def do_or (that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpOr,  (this, that), em)
  def do_xor(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpXor, (this, that), em)

  // Implementations for parent abstracts
  def do_not(em: EnclosingModule): Self = UnaryOp.SInt(OpNot, this, this.getWidth, em)
  def do_neg(em: EnclosingModule): MultiSelf = this.do_sub(LiteralSInt(0), em)
  
  def do_andR(em: EnclosingModule): Bool = this.do_eq( LiteralSInt(-1), em)
  def do_orR (em: EnclosingModule): Bool = this.do_neq(LiteralSInt(0), em)
  
  def do_toUInt(em: EnclosingModule): UInt = UnaryOp.UInt(OpToUInt, this, None, em)
  def do_toSInt(em: EnclosingModule): SInt = UnaryOp.SInt(OpIDENT,  this, None, em)
  def do_asUInt(em: EnclosingModule): UInt = UnaryOp.UInt(OpAsUInt, this, getWidth, em)
  def do_asSInt(em: EnclosingModule): SInt = UnaryOp.SInt(OpIDENT, this, getWidth, em)
  
  // BINARY OPERATIONS
  def do_lshft(that: UInt, em: EnclosingModule): MultiSelf = BinaryOp.SInt(OpLShft, (this, that), em)
  def do_rshft(that: UInt, em: EnclosingModule): MultiSelf = BinaryOp.SInt(OpRShft, (this, that), em)
  
  def do_add(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpPlus, (this, that), em)
  def do_sub(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpSubt, (this, that), em)
  def do_mul(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpMult, (this, that), em)
  def do_div(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpDiv,  (this, that), em)
  def do_mod(that: SInt, em: EnclosingModule): SInt = BinaryOp.SInt(OpMod,  (this, that), em)

  def do_add(that: UIntLike, em: EnclosingModule): MultiSelf = BinaryOp.SInt(OpPlus, (this, that.do_toSInt(em)), em)
  def do_sub(that: UIntLike, em: EnclosingModule): MultiSelf = BinaryOp.SInt(OpSubt, (this, that.do_toSInt(em)), em)
  def do_mul(that: UIntLike, em: EnclosingModule): MultiSelf = BinaryOp.SInt(OpMult, (this, that.do_toSInt(em)), em)
  def do_div(that: UIntLike, em: EnclosingModule): MultiSelf = BinaryOp.SInt(OpDiv,  (this, that.do_toSInt(em)), em)
  def do_mod(that: UIntLike, em: EnclosingModule): MultiSelf = BinaryOp.SInt(OpMod,  (this, that.do_toSInt(em)), em)

  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}
  // External API
  def ===(that: SInt): Bool = macro XFORM.do_eq.thatarg
  def !==(that: SInt): Bool = macro XFORM.do_neq.thatarg

  def   <(that: SInt): Bool = macro XFORM.do_lt.thatarg
  def  <=(that: SInt): Bool = macro XFORM.do_lte.thatarg
  def   >(that: SInt): Bool = macro XFORM.do_gt.thatarg
  def  >=(that: SInt): Bool = macro XFORM.do_gte.thatarg
  
  def   &(that: SInt): SInt = macro XFORM.do_and.thatarg
  def   |(that: SInt): SInt = macro XFORM.do_or.thatarg
  def   ^(that: SInt): SInt = macro XFORM.do_xor.thatarg
}
