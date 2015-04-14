package gama
import internal._

abstract class BaseElem(initialNode: Node) extends Element(initialNode) with ExtractableImpl{
  // Helper operations used internally and possibly externally
  def getWidth: Option[Int] = this.node.storage match {
    case b: RawBits => (b.width)
    case _ => throw new ChiselException("Catastrophic error: BaseElem bound to non-RawBits NodeStorage") {}
  }

  // pseudo-UNARY OPERATIONS
  def extract(position: Int)(implicit em: EnclosingModule): Bool = doExtract(position, em)
  def extract(left_pos: Int, right_pos: Int)(implicit em: EnclosingModule): UInt = doExtract(left_pos, right_pos, em)

  def apply(position: Int, em: EnclosingModule): Bool = extract(position)(em)
  def apply(left_pos: Int, right_pos: Int, em: EnclosingModule): UInt = extract(left_pos, right_pos)(em)

  import scala.language.experimental.macros
  def apply(arg0: Int): Bool = macro macroDef.transformapply1 
  def apply(arg0: Int, arg1: Int): UInt = macro macroDef.transformapply2

  // BINARY OPERATIONS
  // CONCRETE OPERATIONS
  def  ##(that: BaseElem)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpCat, (this, that), em)

  def andR(implicit em: EnclosingModule): Bool
  def  orR(implicit em: EnclosingModule): Bool
  def xorR(implicit em: EnclosingModule): Bool = UnaryOp.Bool(OpXorRed, this, em)

  // ABSTRACT OPERATIONS
  def pad(that: BaseElem)(implicit em: EnclosingModule): BaseElem
  def  <<(that: UInt)(implicit em: EnclosingModule): BaseElem
  
  def toUInt(implicit em: EnclosingModule): UInt
  def toSInt(implicit em: EnclosingModule): SInt
  
  def asUInt(implicit em: EnclosingModule): UInt
  def asSInt(implicit em: EnclosingModule): SInt
  
  def unary_~(implicit em: EnclosingModule): BaseElem

  // DISPATCHED OPERATIONS
  def +(that: BaseElem)(implicit em: EnclosingModule): BaseElem = that match {
    case u: UIntLike => (this + u)
    case s: SInt     => (this + s)
  }
  def -(that: BaseElem)(implicit em: EnclosingModule): BaseElem = that match {
    case u: UIntLike => (this - u)
    case s: SInt     => (this - s)
  }
  def *(that: BaseElem)(implicit em: EnclosingModule): BaseElem = that match {
    case u: UIntLike => (this * u)
    case s: SInt     => (this * s)
  }
  def /(that: BaseElem)(implicit em: EnclosingModule): BaseElem = that match {
    case u: UIntLike => (this / u)
    case s: SInt     => (this / s)
  }
  def %(that: BaseElem)(implicit em: EnclosingModule): BaseElem = that match {
    case u: UIntLike => (this % u)
    case s: SInt     => (this % s)
  }

  // DISPATCH DEPENDENCIES
  def +(that: UIntLike)(implicit em: EnclosingModule): BaseElem
  def -(that: UIntLike)(implicit em: EnclosingModule): BaseElem
  def *(that: UIntLike)(implicit em: EnclosingModule): BaseElem
  def /(that: UIntLike)(implicit em: EnclosingModule): BaseElem
  def %(that: UIntLike)(implicit em: EnclosingModule): BaseElem

  def +(that: SInt    )(implicit em: EnclosingModule): BaseElem
  def -(that: SInt    )(implicit em: EnclosingModule): BaseElem
  def *(that: SInt    )(implicit em: EnclosingModule): BaseElem
  def /(that: SInt    )(implicit em: EnclosingModule): BaseElem
  def %(that: SInt    )(implicit em: EnclosingModule): BaseElem
}

