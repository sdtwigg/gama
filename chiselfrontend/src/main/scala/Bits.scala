package gama
import internal._

abstract class Bits(initialNode: Node) extends Element(initialNode) {
  // Helper operations used internally and possibly externally
  def getWidth: Option[Int] = this.node.storage match {
    case b: RawBits => (b.width)
    case _ => throw new ChiselException("Catastrophic error: Bits bound to non-RawBits NodeStorage") {}
  }

  // pseudo-UNARY OPERATIONS
  def extract(position: Int)(implicit em: EnclosingModule): Bool = ExtractOp.Bool(this, position, em)
  def extract(left_pos: Int, right_pos: Int)(implicit em: EnclosingModule): UInt = ExtractOp.UInt(this, left_pos, right_pos, em)

  def apply(position: Int, em: EnclosingModule): Bool = extract(position)(em)
  def apply(left_pos: Int, right_pos: Int, em: EnclosingModule): UInt = extract(left_pos, right_pos)(em)

  import scala.language.experimental.macros
  def apply(arg0: Int): Bool = macro macroDef.transformapply1 
  def apply(arg0: Int, arg1: Int): UInt = macro macroDef.transformapply2

  // BINARY OPERATIONS
  // CONCRETE OPERATIONS
  def  ##(that: Bits)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpCat,   (this, that), em)

  def ===(that: Bits)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpEqual, (this, that), em)
  def !==(that: Bits)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpNotEq, (this, that), em)
  // TODO: CONSIDER: must these be dispatched and extended? or any conversions applied?

  def andR(implicit em: EnclosingModule): Bool = this === api.S(-1) 
  def  orR(implicit em: EnclosingModule): Bool = this !== api.U(0) 
  def xorR(implicit em: EnclosingModule): Bool = UnaryOp.Bool(OpXorRed, this, em)

  // ABSTRACT OPERATIONS
  def pad(that: Bits)(implicit em: EnclosingModule): Bits
  def  <<(that: UInt)(implicit em: EnclosingModule): Bits
  
  def toUInt(implicit em: EnclosingModule): UInt
  def toSInt(implicit em: EnclosingModule): SInt
  
  def asUInt(implicit em: EnclosingModule): UInt
  def asSInt(implicit em: EnclosingModule): SInt

  // DISPATCHED OPERATIONS
  def +(that: Bits)(implicit em: EnclosingModule): Bits = that match {
    case u: UIntLike => (this + u)
    case s: SInt     => (this + s)
  }
  def -(that: Bits)(implicit em: EnclosingModule): Bits = that match {
    case u: UIntLike => (this - u)
    case s: SInt     => (this - s)
  }
  def *(that: Bits)(implicit em: EnclosingModule): Bits = that match {
    case u: UIntLike => (this * u)
    case s: SInt     => (this * s)
  }
  def /(that: Bits)(implicit em: EnclosingModule): Bits = that match {
    case u: UIntLike => (this / u)
    case s: SInt     => (this / s)
  }
  def %(that: Bits)(implicit em: EnclosingModule): Bits = that match {
    case u: UIntLike => (this % u)
    case s: SInt     => (this % s)
  }
  def &(that: Bits)(implicit em: EnclosingModule): Bits = that match {
    case u: UIntLike => (this | u)
    case s: SInt     => (this | s)
  } // TODO: CONSIDER: always return UIntLike?
  def |(that: Bits)(implicit em: EnclosingModule): Bits = that match {
    case u: UIntLike => (this | u)
    case s: SInt     => (this | s)
  } // TODO: CONSIDER: always return UIntLike?
  def ^(that: Bits)(implicit em: EnclosingModule): Bits = that match {
    case u: UIntLike => (this ^ u)
    case s: SInt     => (this ^ s)
  } // TODO: CONSIDER: always return UIntLike?

  // DISPATCH DEPENDENCIES
  def +(that: UIntLike)(implicit em: EnclosingModule): Bits
  def -(that: UIntLike)(implicit em: EnclosingModule): Bits
  def *(that: UIntLike)(implicit em: EnclosingModule): Bits
  def /(that: UIntLike)(implicit em: EnclosingModule): Bits
  def %(that: UIntLike)(implicit em: EnclosingModule): Bits
  def &(that: UIntLike)(implicit em: EnclosingModule): Bits // TODO: CONSIDER: always return UIntLike?
  def |(that: UIntLike)(implicit em: EnclosingModule): Bits // TODO: CONSIDER: always return UIntLike?
  def ^(that: UIntLike)(implicit em: EnclosingModule): Bits // TODO: CONSIDER: always return UIntLike?

  def +(that: SInt    )(implicit em: EnclosingModule): Bits
  def -(that: SInt    )(implicit em: EnclosingModule): Bits
  def *(that: SInt    )(implicit em: EnclosingModule): Bits
  def /(that: SInt    )(implicit em: EnclosingModule): Bits
  def %(that: SInt    )(implicit em: EnclosingModule): Bits
  def &(that: SInt    )(implicit em: EnclosingModule): Bits // TODO: CONSIDER: always return UIntLike?
  def |(that: SInt    )(implicit em: EnclosingModule): Bits // TODO: CONSIDER: always return UIntLike?
  def ^(that: SInt    )(implicit em: EnclosingModule): Bits // TODO: CONSIDER: always return UIntLike?
}
object Bits {
/*
// Making this available would allow muxing between UInt and SInt (and thus making a Vec of them)
  implicit object selfmuxer extends SelfMuxable[Bits] {
    def mux(tc: Bits, fc: Bits) = ???
  }
// These two allow muxing but not Vec creation
  implicit object usintmuxer1 extends Muxable[UInt, SInt] {
    def mux[D<:Data, A >: UInt <: D, B >: SInt <: D](tc: UInt, fc: SInt): D = ???.asInstanceOf[D]
  }
  implicit object usintmuxer2 extends Muxable[SInt, UInt] {
    def mux[D<:Data, A >: SInt <: D, B >: UInt <: D](tc: SInt, fc: IInt): D = ???.asInstanceOf[D]
  }
*/
}
