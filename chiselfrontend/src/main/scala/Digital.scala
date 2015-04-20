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

