package gama
import internal._

abstract class Digital(initialNode: Node) extends Element(initialNode) with ExtractableImpl {
  // Helper operations used internally and possibly externally
  def getWidth: Option[Int] = this.node.storage match {
    case b: RawBits => (b.width)
    case _ => throw new ChiselException("Catastrophic error: Digital bound to non-RawBits NodeStorage") {}
  }
  // Prepare for macro use
  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}

  // pseudo-UNARY OPERATIONS
  // external -> internal API
  // doExtract (defined by ExtractableImpl)
  
  // External API
  def extract(position: Int): Bool = macro XFORM.doExtract.onearg
  def extract(left_pos: Int, right_pos: Int): UInt = macro XFORM.doExtract.twoarg

  def apply(position: Int): Bool = macro XFORM.doExtract.onearg 
  def apply(left_pos: Int, right_pos: Int): UInt = macro XFORM.doExtract.twoarg

  // BINARY OPERATIONS
  // external->internal API
  // CONCRETE OPERATIONS
  def do_cat(that: Digital, em: EnclosingModule): UInt = BinaryOp.UInt(OpCat, (this, that), em)
  
  def do_andR(em: EnclosingModule): Bool
  def do_orR (em: EnclosingModule): Bool
  def do_xorR(em: EnclosingModule): Bool = UnaryOp.Bool(OpXorRed, this, em)
  
  def do_not(em: EnclosingModule): Digital
 
  // External API
  // CONCRETE OPERATIONS
  def  ##(that: Digital): UInt = macro XFORM.do_cat.thatarg

  def andR: Bool = macro XFORM.do_andR.noparen
  def  orR: Bool = macro XFORM.do_orR.noparen
  def xorR: Bool = macro XFORM.do_xorR.noparen
  
  def unary_~(): Digital = macro XFORM.do_not.paren

  // TO BE CONVERTED VVVV
  // ABSTRACT OPERATIONS
  def pad(that: Digital)(implicit em: EnclosingModule): Digital
  def  <<(that: UInt)(implicit em: EnclosingModule): Digital
  def  >>(that: UInt)(implicit em: EnclosingModule): Digital
  
  def toUInt(implicit em: EnclosingModule): UInt
  def toSInt(implicit em: EnclosingModule): SInt
  
  def asUInt(implicit em: EnclosingModule): UInt
  def asSInt(implicit em: EnclosingModule): SInt
  
  def unary_-(implicit em: EnclosingModule): Digital

  // DISPATCHED OPERATIONS
  def +(that: Digital)(implicit em: EnclosingModule): Digital = that match {
    case u: UIntLike => (this + u)
    case s: SInt     => (this + s)
  }
  def -(that: Digital)(implicit em: EnclosingModule): Digital = that match {
    case u: UIntLike => (this - u)
    case s: SInt     => (this - s)
  }
  def *(that: Digital)(implicit em: EnclosingModule): Digital = that match {
    case u: UIntLike => (this * u)
    case s: SInt     => (this * s)
  }
  def /(that: Digital)(implicit em: EnclosingModule): Digital = that match {
    case u: UIntLike => (this / u)
    case s: SInt     => (this / s)
  }
  def %(that: Digital)(implicit em: EnclosingModule): Digital = that match {
    case u: UIntLike => (this % u)
    case s: SInt     => (this % s)
  }

  // DISPATCH DEPENDENCIES
  def +(that: UIntLike)(implicit em: EnclosingModule): Digital
  def -(that: UIntLike)(implicit em: EnclosingModule): Digital
  def *(that: UIntLike)(implicit em: EnclosingModule): Digital
  def /(that: UIntLike)(implicit em: EnclosingModule): Digital
  def %(that: UIntLike)(implicit em: EnclosingModule): Digital

  def +(that: SInt    )(implicit em: EnclosingModule): Digital
  def -(that: SInt    )(implicit em: EnclosingModule): Digital
  def *(that: SInt    )(implicit em: EnclosingModule): Digital
  def /(that: SInt    )(implicit em: EnclosingModule): Digital
  def %(that: SInt    )(implicit em: EnclosingModule): Digital
}

