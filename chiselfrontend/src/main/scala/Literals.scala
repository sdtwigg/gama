package gama
import internal._

// BOOL LITERALS
object LiteralBool {
  val True: Bool  = LitDesc(Bool(), BoolLitMap(true)).retVal
  val False: Bool = LitDesc(Bool(), BoolLitMap(false)).retVal
  
  def apply(value: Boolean): Bool = if(value) True else False
}

trait LitIntObjectBase[Out<:Digital] {
  def apply(value: BigInt, width: Int): Out
  def apply(value: BigInt): Out
}

// INT LITERALS
object LiteralSInt extends LitIntObjectBase[SInt] {
  def apply(value: BigInt, width: Int): SInt =
    LitDesc(SInt(), SIntLitMap(value, Some(width))).retVal
  def apply(value: BigInt): SInt = 
    LitDesc(SInt(), SIntLitMap(value, None)).retVal
}

object LiteralUInt extends LitIntObjectBase[UInt] {
  def apply(value: BigInt, width: Int): UInt = 
    LitDesc(UInt(), UIntLitMap(value, Some(width))).retVal
  def apply(value: BigInt): UInt = 
    LitDesc(UInt(), UIntLitMap(value, None)).retVal
}

// BASIC VEC LITERALS
object LiteralVec {
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
