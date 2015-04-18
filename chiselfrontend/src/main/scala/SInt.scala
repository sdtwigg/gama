package gama
import internal._

object SInt {
  def apply(): SInt           = apply(None)
  def apply(width: Int): SInt = apply(Some(width))
  def apply(width: Option[Int]) = new SInt(new SPEC(SBits(width), None))

  implicit object basicfunctionality extends Muxable[SInt] with Element.ConnectToImpl[SInt,Digital] {
    def muxRetVal(tc: SInt, fc: SInt): SInt = SInt()
  }
}
final class SInt(initialNode: Node) extends Digital(initialNode) {
  // GENERAL ELEMENT REQUIREMENTS
  def :=(source: Digital)(implicit em: EnclosingModule): Unit =
    ConnectTo[SInt,Digital].connect(Sink(this), Source(source), em)
  def copy = new SInt(new SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
  
  // IMPLEMENT EQUALITY AND OTHER COMPARISONS
  def ===(that: SInt)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpEqual, (this, that), em)
  def !==(that: SInt)(implicit em: EnclosingModule): Bool = BinaryOp.Bool(OpNotEq, (this, that), em)
  
  // IMPLEMENT SIMPLE ABSTRACT OPERATIONS
  def andR(implicit em: EnclosingModule): Bool = this === api.S(-1)
  def orR(implicit em: EnclosingModule): Bool  = this !== api.S(0)
  
  def pad(that: Digital)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpPadTo,  (this, that), em)
  def  <<(that: UInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpLShft, (this, that), em)
  
  def toUInt(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpToUInt, this, None, em)
  def toSInt(implicit em: EnclosingModule): SInt = UnaryOp.SInt(OpIDENT,  this, None, em)
  
  def asUInt(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpAsUInt, this, getWidth, em)
  def asSInt(implicit em: EnclosingModule): SInt = UnaryOp.SInt(OpIDENT,  this, getWidth, em)
  
  def unary_~(implicit em: EnclosingModule): SInt = UnaryOp.SInt(OpNot, this, this.getWidth, em)

  // IMPLEMENT OPERATIONS WITH SELF
  def +(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpPlus, (this, that), em)
  def -(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpSubt, (this, that), em)
  def *(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpMult, (this, that), em)
  def /(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpDiv,  (this, that), em)
  def %(that: SInt)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpMod,  (this, that), em)

  // IMPLEMENT OPERATIONS WITH OTHERS
  def +(that: UIntLike)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpPlus, (this, that.toSInt), em)
  def -(that: UIntLike)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpSubt, (this, that.toSInt), em)
  def *(that: UIntLike)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpMult, (this, that.toSInt), em)
  def /(that: UIntLike)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpDiv,  (this, that.toSInt), em)
  def %(that: UIntLike)(implicit em: EnclosingModule): SInt = BinaryOp.SInt(OpMod,  (this, that.toSInt), em)
}
