package gama
import internal._

abstract class Digital(initialNode: Node) extends Element(initialNode) with ExtractableImpl {
  protected type Self <: Digital // Traces all the way down to concrete
  protected type MultiSelf <: Digital // Can hold multiple bits

  // Helper operations used internally and possibly externally
  def getWidth: Option[Int] = this.node.storage match {
    case b: RawBits => (b.width)
    case _ => throw new ChiselException("Catastrophic error: Digital bound to non-RawBits NodeStorage") {}
  }
  // Prepare for macro use
  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}
  
  // External API
  // pseudo-UNARY OPERATIONS
  def extract(position: Int): Bool = macro XFORM.doExtract.onearg
  def extract(left_pos: Int, right_pos: Int): UInt = macro XFORM.doExtract.twoarg

  def apply(position: Int): Bool = macro XFORM.doExtract.onearg 
  def apply(left_pos: Int, right_pos: Int): UInt = macro XFORM.doExtract.twoarg
  
  // UNARY OPERATIONS
  def andR: Bool = macro XFORM.do_andR.noparen
  def  orR: Bool = macro XFORM.do_orR.noparen
  def xorR: Bool = macro XFORM.do_xorR.noparen
  
  def unary_~(): Self      = macro XFORM.do_not.paren
  def unary_-(): MultiSelf = macro XFORM.do_neg.paren
  
  def toUInt: UInt = macro XFORM.do_toUInt.noparen
  def toSInt: SInt = macro XFORM.do_toSInt.noparen
  
  def asUInt: UInt = macro XFORM.do_asUInt.noparen
  def asSInt: SInt = macro XFORM.do_asSInt.noparen
  
  // BINARY OPERATIONS
  def  ##(that: Digital): UInt   = macro XFORM.do_cat.thatarg
  def  <<(that: UInt): MultiSelf = macro XFORM.do_lshft.thatarg
  def  >>(that: UInt): MultiSelf = macro XFORM.do_rshft.thatarg
  
  def +(that: UIntLike): MultiSelf = macro XFORM.do_add.thatarg
  def -(that: UIntLike): MultiSelf = macro XFORM.do_sub.thatarg
  def *(that: UIntLike): MultiSelf = macro XFORM.do_mul.thatarg
  def /(that: UIntLike): MultiSelf = macro XFORM.do_div.thatarg
  def %(that: UIntLike): MultiSelf = macro XFORM.do_mod.thatarg

  def +(that: SInt): SInt = macro XFORM.do_add.thatarg
  def -(that: SInt): SInt = macro XFORM.do_sub.thatarg
  def *(that: SInt): SInt = macro XFORM.do_mul.thatarg
  def /(that: SInt): SInt = macro XFORM.do_div.thatarg
  def %(that: SInt): SInt = macro XFORM.do_mod.thatarg
 
  // Dispatch for these
  def +(that: Digital): Digital = macro XFORM.do_add.thatarg
  def -(that: Digital): Digital = macro XFORM.do_sub.thatarg
  def *(that: Digital): Digital = macro XFORM.do_mul.thatarg
  def /(that: Digital): Digital = macro XFORM.do_div.thatarg
  def %(that: Digital): Digital = macro XFORM.do_mod.thatarg

  // external -> internal API
  // pseudo-UNARY OPERATIONS
  // doExtract (defined by ExtractableImpl)
  
  // UNARY OPERATIONS
  def do_andR(em: EnclosingModule): Bool
  def do_orR (em: EnclosingModule): Bool
  def do_xorR(em: EnclosingModule): Bool = UnaryOp.Bool(OpXorRed, this, em)
  
  def do_not(em: EnclosingModule): Self
  def do_neg(em: EnclosingModule): MultiSelf
  
  def do_toUInt(em: EnclosingModule): UInt
  def do_toSInt(em: EnclosingModule): SInt
  def do_asUInt(em: EnclosingModule): UInt
  def do_asSInt(em: EnclosingModule): SInt
  
  // BINARY OPERATIONS
  def do_cat(that: Digital, em: EnclosingModule): UInt = BinaryOp.UInt(OpCat, (this, that), em)
  def do_lshft(that: UInt, em: EnclosingModule): MultiSelf
  def do_rshft(that: UInt, em: EnclosingModule): MultiSelf
  
  def do_add(that: UIntLike, em: EnclosingModule): MultiSelf
  def do_sub(that: UIntLike, em: EnclosingModule): MultiSelf
  def do_mul(that: UIntLike, em: EnclosingModule): MultiSelf
  def do_div(that: UIntLike, em: EnclosingModule): MultiSelf
  def do_mod(that: UIntLike, em: EnclosingModule): MultiSelf

  def do_add(that: SInt, em: EnclosingModule): SInt
  def do_sub(that: SInt, em: EnclosingModule): SInt
  def do_mul(that: SInt, em: EnclosingModule): SInt
  def do_div(that: SInt, em: EnclosingModule): SInt
  def do_mod(that: SInt, em: EnclosingModule): SInt
  
  // DISPATCHED OPERATIONS
  def do_add(that: Digital, em: EnclosingModule): Digital = that match {
    case u: UIntLike => (this.do_add(u, em))
    case s: SInt     => (this.do_add(s, em))
  }
  def do_sub(that: Digital, em: EnclosingModule): Digital = that match {
    case u: UIntLike => (this.do_sub(u, em))
    case s: SInt     => (this.do_sub(s, em))
  }
  def do_mul(that: Digital, em: EnclosingModule): Digital = that match {
    case u: UIntLike => (this.do_mul(u, em))
    case s: SInt     => (this.do_mul(s, em))
  }
  def do_div(that: Digital, em: EnclosingModule): Digital = that match {
    case u: UIntLike => (this.do_div(u, em))
    case s: SInt     => (this.do_div(s, em))
  }
  def do_mod(that: Digital, em: EnclosingModule): Digital = that match {
    case u: UIntLike => (this.do_mod(u, em))
    case s: SInt     => (this.do_mod(s, em))
  }
 
}

