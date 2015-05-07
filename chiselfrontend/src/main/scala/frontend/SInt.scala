package gama
package frontend

import implementation._
  
// Activate Scala Language Features
import scala.language.experimental.macros
import implementation.macrodefs.{TransformMacro => XFORM}

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
  // OKAY-ness discussed in BiConnect.scala
}

final class SInt(initialNode: Node) extends Digital(initialNode) {
  protected type Self = SInt
  protected type MultiSelf = SInt // Can hold multiple bits
  
  def copy = new SInt(new SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]

  // External API
  def :=(source: Digital): Unit = macro XFORM.doConnectTo.sourcearg
  def <>(right:  SInt): Unit = macro XFORM.doBiConnect.rightarg
  def <>(right:  UInt): Unit = macro XFORM.doBiConnect.rightarg
  // TODO: there are ways to reduce duplication here
  //    main disadvantage is cleaning up error messages when user does wrong thing
  
  def ===(that: SInt): Bool = macro XFORM.do_eq.thatarg
  def !==(that: SInt): Bool = macro XFORM.do_neq.thatarg

  def   <(that: SInt): Bool = macro XFORM.do_lt.thatarg
  def  <=(that: SInt): Bool = macro XFORM.do_lte.thatarg
  def   >(that: SInt): Bool = macro XFORM.do_gt.thatarg
  def  >=(that: SInt): Bool = macro XFORM.do_gte.thatarg
  
  def   &(that: SInt): SInt = macro XFORM.do_and.thatarg
  def   |(that: SInt): SInt = macro XFORM.do_or.thatarg
  def   ^(that: SInt): SInt = macro XFORM.do_xor.thatarg
  
  // external->internal API
  // New operations
  def doConnectTo(source: Digital, info: EnclosureInfo) = 
    ConnectTo[SInt,Digital].monoConnect(Sink(this), Source(source), info)
  def doBiConnect(right: SInt, info: EnclosureInfo): Unit =
    BiConnect[SInt,SInt].biConnect(Left(this), Right(right), info)
  def doBiConnect(right: UInt, info: EnclosureInfo): Unit =
    BiConnect[SInt,UInt].biConnect(Left(this), Right(right), info)
  // TODO: See above
  
  def do_eq (that: SInt, info: EnclosureInfo): Bool = BinaryOp.Bool(OpEqual, (this, that), info)
  def do_neq(that: SInt, info: EnclosureInfo): Bool = BinaryOp.Bool(OpNotEq, (this, that), info)
  
  def do_lt (that: SInt, info: EnclosureInfo): Bool = BinaryOp.Bool(OpLess,  (this, that), info)
  def do_lte(that: SInt, info: EnclosureInfo): Bool = BinaryOp.Bool(OpLeEq,  (this, that), info)
  def do_gt (that: SInt, info: EnclosureInfo): Bool = BinaryOp.Bool(OpGrt,   (this, that), info)
  def do_gtw(that: SInt, info: EnclosureInfo): Bool = BinaryOp.Bool(OpGrEq,  (this, that), info)
  
  def do_and(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpAnd, (this, that), info)
  def do_or (that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpOr,  (this, that), info)
  def do_xor(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpXor, (this, that), info)

  // Implementations for parent abstracts
  def do_connectFromUInt(in: UInt, info: EnclosureInfo): this.type = {
    this.doConnectTo(in.do_asSInt(info), info)
    this
  }
  
  def do_not(info: EnclosureInfo): Self = UnaryOp.SInt(OpNot, this, this.getWidth, info)
  def do_neg(info: EnclosureInfo): MultiSelf = this.do_sub(LiteralSInt(0), info)
  
  def do_andR(info: EnclosureInfo): Bool = this.do_eq( LiteralSInt(-1), info)
  def do_orR (info: EnclosureInfo): Bool = this.do_neq(LiteralSInt(0), info)
  
  def do_toUInt(info: EnclosureInfo): UInt = UnaryOp.UInt(OpToUInt, this, None, info)
  def do_toSInt(info: EnclosureInfo): SInt = UnaryOp.SInt(OpIDENT,  this, None, info)
  def do_asUInt(info: EnclosureInfo): UInt = UnaryOp.UInt(OpAsUInt, this, getWidth, info)
  def do_asSInt(info: EnclosureInfo): SInt = UnaryOp.SInt(OpIDENT, this, getWidth, info)
  
  // BINARY OPERATIONS
  def do_lshft(that: UInt, info: EnclosureInfo): MultiSelf =
    that.getLiteralValue.map(this.do_lshft(_, info)).getOrElse(BinaryOp.SInt(OpLShft, (this, that), info))
  def do_lshft(that: Int,  info: EnclosureInfo): MultiSelf = this.do_cat(LiteralUInt(0, that), info).do_asSInt(info)
  def do_rshft(that: UInt, info: EnclosureInfo): MultiSelf = BinaryOp.SInt(OpRShft, (this, that), info)
  
  def do_add(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpPlus, (this, that), info)
  def do_sub(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpSubt, (this, that), info)
  def do_mul(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpMult, (this, that), info)
  def do_div(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpDiv,  (this, that), info)
  def do_mod(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpMod,  (this, that), info)

  def do_add(that: UIntLike, info: EnclosureInfo): MultiSelf = BinaryOp.SInt(OpPlus, (this, that.do_toSInt(info)), info)
  def do_sub(that: UIntLike, info: EnclosureInfo): MultiSelf = BinaryOp.SInt(OpSubt, (this, that.do_toSInt(info)), info)
  def do_mul(that: UIntLike, info: EnclosureInfo): MultiSelf = BinaryOp.SInt(OpMult, (this, that.do_toSInt(info)), info)
  def do_div(that: UIntLike, info: EnclosureInfo): MultiSelf = BinaryOp.SInt(OpDiv,  (this, that.do_toSInt(info)), info)
  def do_mod(that: UIntLike, info: EnclosureInfo): MultiSelf = BinaryOp.SInt(OpMod,  (this, that.do_toSInt(info)), info)
}
