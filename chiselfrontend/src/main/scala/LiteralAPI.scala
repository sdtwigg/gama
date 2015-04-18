package gama
import internal._

// BOOLEAN LITERALS
trait LiteralBoolAPI {
  def True:  Bool = LiteralBool.actualTrue
  def False: Bool = LiteralBool.actualFalse
}
object LiteralBool {
  val actualTrue: Bool  = LitDesc(Bool(), BoolLitMap(true)).retVal
  val actualFalse: Bool = LitDesc(Bool(), BoolLitMap(false)).retVal
}
trait LitBoolObjectImpl {
  def apply(value: Boolean): Bool =
    if(value) LiteralBool.actualTrue else LiteralBool.actualFalse
}

// INTEGER LITERALS
trait LitIntObjectBase[Out<:Digital] {
  def apply(value: BigInt, width: Int): Out
  def apply(value: BigInt): Out
}

trait LitSIntObjectImpl extends LitIntObjectBase[SInt] {
  def apply(value: BigInt, width: Int): SInt =
    LitDesc(SInt(), SIntLitMap(value, Some(width))).retVal
  def apply(value: BigInt): SInt = 
    LitDesc(SInt(), SIntLitMap(value, None)).retVal
}
trait LitUIntObjectImpl extends LitIntObjectBase[UInt] {
  def apply(value: BigInt, width: Int): UInt = 
    LitDesc(UInt(), UIntLitMap(value, Some(width))).retVal
  def apply(value: BigInt): UInt = 
    LitDesc(UInt(), UIntLitMap(value, None)).retVal
}

// BASIC VEC LITERALS
class LitVecObjectImpl {
  def U(elt0: Int, elts: Int*): Vec[UInt] = U(elt0 +: (elts.toSeq))
  def U(elts: Iterable[Int]): Vec[UInt] = {
    val litmaps: Seq[LitMap[UInt]] = elts.map(i => UIntLitMap(i, None)).toSeq
    LitDesc(Vec(litmaps.length, UInt()), VecLitMap(litmaps)).retVal
  }
  // TODO: Some way to construct arbitrary Vec of Vec of...
  //    Perhaps some function that just returns the litmap
  //    then a function to take the seq of litmap and turn into vec
  def S(elt0: Int, elts: Int*): Vec[SInt] = S(elt0 +: (elts.toSeq))
  def S(elts: Iterable[Int]): Vec[SInt] = {
    val litmaps: Seq[LitMap[SInt]] = elts.map(i => SIntLitMap(i, None)).toSeq
    LitDesc(Vec(litmaps.length, SInt()), VecLitMap(litmaps)).retVal
  }
}
