package gama
package frontend

import implementation._

// Activate Scala Language Features
import scala.language.experimental.macros
import implementation.macrodefs.{TransformMacro => XFORM}

object UIntLike {
  implicit object basicfunctionality extends Muxable[UIntLike] {
    def muxRetVal(tc: UIntLike, fc: UIntLike) = UInt()
  }
}
abstract class UIntLike(initialNode: Node) extends Digital(initialNode) {
  protected type Self <: UIntLike
  protected type MultiSelf = UInt // Can hold multiple bits
  
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

  // external -> internal API
  // New operations
  def do_eq (that: UIntLike, info: EnclosureInfo): Bool = BinaryOp.Bool(OpEqual, (this, that), info)
  def do_neq(that: UIntLike, info: EnclosureInfo): Bool = BinaryOp.Bool(OpNotEq, (this, that), info)
  
  def do_lt (that: UIntLike, info: EnclosureInfo): Bool = BinaryOp.Bool(OpLess,  (this, that), info)
  def do_lte(that: UIntLike, info: EnclosureInfo): Bool = BinaryOp.Bool(OpLeEq,  (this, that), info)
  def do_gt (that: UIntLike, info: EnclosureInfo): Bool = BinaryOp.Bool(OpGrt,   (this, that), info)
  def do_gtw(that: UIntLike, info: EnclosureInfo): Bool = BinaryOp.Bool(OpGrEq,  (this, that), info)
  
  def do_and(that: UIntLike, info: EnclosureInfo): UInt = BinaryOp.UInt(OpAnd, (this, that), info)
  def do_or (that: UIntLike, info: EnclosureInfo): UInt = BinaryOp.UInt(OpOr,  (this, that), info)
  def do_xor(that: UIntLike, info: EnclosureInfo): UInt = BinaryOp.UInt(OpXor, (this, that), info)
  
  // Implementations for parent abstracts
  def do_andR(info: EnclosureInfo): Bool = this.do_eq( LiteralUInt(0), info).do_not(info)
  def do_orR (info: EnclosureInfo): Bool = this.do_neq(LiteralUInt(0), info)
  
  def do_neg(info: EnclosureInfo): MultiSelf = BinaryOp.UInt(OpSubt, (api.U(0),this), info) 
  
  def do_toUInt(info: EnclosureInfo): UInt = UnaryOp.UInt(OpIDENT,  this, None, info)
  def do_toSInt(info: EnclosureInfo): SInt = UnaryOp.SInt(OpToSInt, this, None, info)
  def do_asUInt(info: EnclosureInfo): UInt = UnaryOp.UInt(OpIDENT,  this, getWidth, info)
  def do_asSInt(info: EnclosureInfo): SInt = UnaryOp.SInt(OpAsSInt, this, getWidth, info)
  
  // BINARY OPERATIONS
  def do_lshft(that: UInt, info: EnclosureInfo): MultiSelf =
    that.getLiteralValue.map(this.do_lshft(_, info)).getOrElse(BinaryOp.UInt(OpLShft, (this, that), info))
  def do_lshft(that: Int,  info: EnclosureInfo): MultiSelf = this.do_cat(LiteralUInt(0, that), info)
  def do_rshft(that: UInt, info: EnclosureInfo): MultiSelf = BinaryOp.UInt(OpRShft, (this, that), info)
  
  def do_add(that: UIntLike, info: EnclosureInfo): MultiSelf = BinaryOp.UInt(OpPlus, (this, that), info)
  def do_sub(that: UIntLike, info: EnclosureInfo): MultiSelf = BinaryOp.UInt(OpSubt, (this, that), info)
  def do_mul(that: UIntLike, info: EnclosureInfo): MultiSelf = BinaryOp.UInt(OpMult, (this, that), info)
  def do_div(that: UIntLike, info: EnclosureInfo): MultiSelf = BinaryOp.UInt(OpDiv,  (this, that), info)
  def do_mod(that: UIntLike, info: EnclosureInfo): MultiSelf = BinaryOp.UInt(OpMod,  (this, that), info)

  def do_add(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpPlus, (this.do_toSInt(info), that), info)
  def do_sub(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpSubt, (this.do_toSInt(info), that), info)
  def do_mul(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpMult, (this.do_toSInt(info), that), info)
  def do_div(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpDiv,  (this.do_toSInt(info), that), info)
  def do_mod(that: SInt, info: EnclosureInfo): SInt = BinaryOp.SInt(OpMod,  (this.do_toSInt(info), that), info)
}
