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
    SIntLitMap(value, width).manifest
  }
  def apply(value: BigInt): SInt = {
    SIntLitMap(value, value.bitLength+1).manifest
  }
}

object LiteralUInt extends LitIntObjectBase[UInt] {
  def apply(value: BigInt, width: Int): UInt = {
    UIntLitMap(value, width).manifest
  }
  def apply(value: BigInt): UInt = { 
    UIntLitMap(value, value.bitLength).manifest
  }
}

// BASIC VEC LITERALS
object LiteralVec {
  // TODO: Some way to construct arbitrary Vec of Vec of...
  //    Perhaps some function that just returns the litmap
  //    then a function to take the seq of litmap and turn into vec
  
  def U(elt0: BigInt, elts: BigInt*): Vec[UInt] = U(elt0 +: (elts.toSeq))
  def U(elts: Iterable[BigInt]): Vec[UInt] = {
    VecLitMap(elts.map(i => UIntLitMap(i, i.bitLength)).toSeq).manifest
  }
  def S(elt0: BigInt, elts: BigInt*): Vec[SInt] = S(elt0 +: (elts.toSeq))
  def S(elts: Iterable[BigInt]): Vec[SInt] = {
    VecLitMap(elts.map(i => SIntLitMap(i, i.bitLength+1)).toSeq).manifest
  }

  def SW(elt0: Tuple2[BigInt,Int], elts: Tuple2[BigInt,Int]*): Vec[SInt] = SW(elt0 +: (elts.toSeq))
  def SW(elts: Iterable[Tuple2[BigInt, Int]]): Vec[SInt] = {
    VecLitMap(elts.map(i => SIntLitMap(i._1, i._2)).toSeq).manifest
  }
}

