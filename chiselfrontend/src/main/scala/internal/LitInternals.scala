package gama
package internal

trait LitIntObjectBase[Out<:Bits] {
  def apply(rawValue: BigInt, width: Int): Out
  def apply(rawValue: BigInt): Out
}

trait LitSIntObjectImpl extends LitIntObjectBase[SInt] {
  def apply(rawValue: BigInt, width: Int): SInt = ???
  def apply(rawValue: BigInt): SInt = ???
}
trait LitUIntObjectImpl extends LitIntObjectBase[UInt] {
  def apply(rawValue: BigInt, width: Int): UInt = ???
  def apply(rawValue: BigInt): UInt = ???
}
trait LitBoolObjectImpl {
  def True:  Bool = ???
  def False: Bool = ???
}

trait LitDescImpl[T<:Data] {
  self: LitDesc[T] => 
    def validateRetVal(): Unit = NodeCheck.assertLitNode(retVal)
    def genJournalEntry = None

    litMap.setup(retVal)
} // Perhaps just a thin wrapper around the LitMap workhorse?

// NEITHER OF THESE CAN BE FINAL SINCE USER MAY EXTEND
//   THIS IS PARTICULARLY TRUE FOR LitMap
// This trait actually holds the literal (hah) information for LitDesc so that
//   it can map details from whatever T is to its constituent elements
trait LitMap[T<:Data] {
  def setup(in: T): in.type
}
// eg. to map to a vector, may take a Seq of LitMaps to the constituent elements
// may need to bypass the normal bind function....

// This class may end up being unnecessary as the information for it is
//   derived from LitMap[T] anyway
//trait LitNodeDesc[NS<:NodeStore]
//case object LitUBits(value: BigInt) extends LitNodeDesc

case class BoolLitMap(value: Boolean) extends LitMap[Bool] {
  def setup(in: Bool): in.type = {
    ???
    in
  }
}
/*
case class UIntLitMap(???) extends ListMap[UInt]
case class SIntLitMap(???) extends ListMap[SInt]
case class Vec[D](???) extends ListMap[Vec[D]]
case class Vec(???) extends ListMap[HardwareTuple]
*/

