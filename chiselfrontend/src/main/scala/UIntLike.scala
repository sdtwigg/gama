package gama
import internal._

object UIntLike {
  implicit object basicfunctionality extends SelfMuxable[UIntLike] with Element.ConnectSelfImpl[UIntLike] {
    def muxRetVal(tc: UIntLike, fc: UIntLike) = UInt()
  }
}
abstract class UIntLike(initialNode: Node) extends Bits(initialNode) {
  def +(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpPlus, (this, that), em)
  def -(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpSubt, (this, that), em)
  def *(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpMult, (this, that), em)
  def /(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpDiv,  (this, that), em)
  def %(that: UIntLike)(implicit em: EnclosingModule): UInt = BinaryOp.UInt(OpMod,  (this, that), em)

  def pad(that: Bits)(implicit em: EnclosingModule): UInt  = BinaryOp.UInt(OpPadTo, (this, that), em)
  def  ##(that: Bits)(implicit em: EnclosingModule): UInt  = BinaryOp.UInt(OpCat,   (this, that), em)

  def ===(that: UIntLike)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpEqual, (this, that), em)
  def !==(that: UIntLike)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpNoneq, (this, that), em)
  def   <(that: UIntLike)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpLess,  (this, that), em)
  def  <=(that: UIntLike)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpLeEq,  (this, that), em)
  def   >(that: UIntLike)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpGrt,   (this, that), em)
  def  >=(that: UIntLike)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpGrEq,  (this, that), em)
}
