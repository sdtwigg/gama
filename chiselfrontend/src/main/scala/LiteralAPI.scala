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

// INTEGER LITERALS
trait LitIntObjectBase[Out<:Bits] {
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
