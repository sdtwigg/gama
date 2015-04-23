package gama
import internal._

// BOOL LITERALS
object LiteralBool {
  val True: Bool  = BoolLitMap(true).manifest
  val False: Bool = BoolLitMap(false).manifest
  
  def apply(value: Boolean): Bool = if(value) True else False
}

trait LitIntObjectBase[Out<:Digital] {
  def apply(value: BigInt, width: Int): Out
  def apply(value: BigInt): Out
}

// INT LITERALS
object LiteralSInt extends LitIntObjectBase[SInt] {
  def apply(value: BigInt, width: Int): SInt = {
    SIntLitMap(value, Some(width)).manifest
  }
  def apply(value: BigInt): SInt = {
    SIntLitMap(value, None).manifest
  }
}

object LiteralUInt extends LitIntObjectBase[UInt] {
  def apply(value: BigInt, width: Int): UInt = {
    UIntLitMap(value, Some(width)).manifest
  }
  def apply(value: BigInt): UInt = { 
    UIntLitMap(value, None).manifest
  }
}

// BASIC VEC LITERALS
object LiteralVec {
  // TODO: Some way to construct arbitrary Vec of Vec of...
  //    Perhaps some function that just returns the litmap
  //    then a function to take the seq of litmap and turn into vec
  
  def U(elt0: Int, elts: Int*): Vec[UInt] = U(elt0 +: (elts.toSeq))
  def U(elts: Iterable[Int]): Vec[UInt] = {
    VecLitMap(elts.map(i => UIntLitMap(i, None)).toSeq).manifest
  }
  def S(elt0: Int, elts: Int*): Vec[SInt] = S(elt0 +: (elts.toSeq))
  def S(elts: Iterable[Int]): Vec[SInt] = {
    VecLitMap(elts.map(i => SIntLitMap(i, None)).toSeq).manifest
  }

  def SW(elt0: Tuple2[Int,Int], elts: Tuple2[Int,Int]*): Vec[SInt] = SW(elt0 +: (elts.toSeq))
  def SW(elts: Iterable[Tuple2[Int, Int]]): Vec[SInt] = {
    VecLitMap(elts.map(i => SIntLitMap(i._1, Some(i._2))).toSeq).manifest
  }
}
