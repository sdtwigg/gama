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
  def extract(position: Int)(implicit em: EnclosingModule): Bool = doExtract(position, em)
  def extract(left_pos: Int, right_pos: Int)(implicit em: EnclosingModule): UInt = doExtract(left_pos, right_pos, em)

  def apply(position: Int, em: EnclosingModule): Bool = extract(position)(em)
  def apply(left_pos: Int, right_pos: Int, em: EnclosingModule): UInt = extract(left_pos, right_pos)(em)

  def apply(arg0: Int): Bool = macro XFORM.to_apply1 
  def apply(arg0: Int, arg1: Int): UInt = macro XFORM.to_apply2

  // BINARY OPERATIONS
  // CONCRETE OPERATIONS
  def  ##(that: Digital)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpCat, (this, that), em)

  def andR(implicit em: EnclosingModule): Bool
  def  orR(implicit em: EnclosingModule): Bool
  def xorR(implicit em: EnclosingModule): Bool = UnaryOp.Bool(OpXorRed, this, em)

  // ABSTRACT OPERATIONS
  def pad(that: Digital)(implicit em: EnclosingModule): Digital
  def  <<(that: UInt)(implicit em: EnclosingModule): Digital
  def  >>(that: UInt)(implicit em: EnclosingModule): Digital
  
  def toUInt(implicit em: EnclosingModule): UInt
  def toSInt(implicit em: EnclosingModule): SInt
  
  def asUInt(implicit em: EnclosingModule): UInt
  def asSInt(implicit em: EnclosingModule): SInt
  
  def unary_~(implicit em: EnclosingModule): Digital
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

