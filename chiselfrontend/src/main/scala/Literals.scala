package gama
import internal._

object LitMapToData {
  def apply[D<:Data](litmap: LitMap[D]): D = LitDesc(litmap.constructData, litmap).retVal
  // TODO: PERHAPS LitMap should make the LitDesc itself....
}

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
  def apply(value: BigInt, width: Int): SInt = {
    LitMapToData( SIntLitMap(value, Some(width)) )
  }
  def apply(value: BigInt): SInt = {
    LitMapToData( SIntLitMap(value, None) )
  }
}

object LiteralUInt extends LitIntObjectBase[UInt] {
  def apply(value: BigInt, width: Int): UInt = {
    LitMapToData( UIntLitMap(value, Some(width)) )
  }
  def apply(value: BigInt): UInt = { 
    LitMapToData( UIntLitMap(value, None) )
  }
}

// BASIC VEC LITERALS
object LiteralVec {
  // TODO: Some way to construct arbitrary Vec of Vec of...
  //    Perhaps some function that just returns the litmap
  //    then a function to take the seq of litmap and turn into vec
  
  def U(elt0: Int, elts: Int*): Vec[UInt] = U(elt0 +: (elts.toSeq))
  def U(elts: Iterable[Int]): Vec[UInt] = {
    LitMapToData( VecLitMap(elts.map(i => UIntLitMap(i, None)).toSeq) )
  }
  def S(elt0: Int, elts: Int*): Vec[SInt] = S(elt0 +: (elts.toSeq))
  def S(elts: Iterable[Int]): Vec[SInt] = {
    LitMapToData( VecLitMap(elts.map(i => SIntLitMap(i, None)).toSeq) )
  }

  def SW(elt0: Tuple2[Int,Int], elts: Tuple2[Int,Int]*): Vec[SInt] = SW(elt0 +: (elts.toSeq))
  def SW(elts: Iterable[Tuple2[Int, Int]]): Vec[SInt] = {
    LitMapToData( VecLitMap(elts.map(i => SIntLitMap(i._1, Some(i._2))).toSeq) )
  }
}
