package gama
package internal

trait LitDescImpl[T<:Data] {
  self: LitDesc[T] => 
    def validateRetVal(): Unit = NodeCheck.assertLitNode(retVal)

    def doWork(): Unit = {
      litMap.setup(retVal, this)
    }
} // A thin wrapper around the LitMap workhorse

// THIS CANNOT BE TOTALLY FINAL SINCE USER MAY EXTEND
// This trait actually holds the literal (hah) information for LitDesc so that
//   it can map details from whatever T is to its constituent elements
sealed abstract class LitMap[T<:Data] {
  def setup(in: T, litdesc: LitDesc[T]): in.type
  def generalizeWith(in: LitMap[T]) = ???
    // Vec uses this to ensure underlying types are the same
}
// eg. to map to a vector, may take a Seq of LitMaps to the constituent elements

// This class may end up being unnecessary as the information for it is
//   derived from LitMap[T] anyway
//trait LitNodeDesc[NS<:NodeStore]
//case object LitUBits(value: BigInt) extends LitNodeDesc

// Recall that each layer of a 'nested' literal has own name and LitDesc
//   so do not bother propogating names and desc
trait LitMapElementImpl[T<:Element] {
  self: LitMap[T] =>
    def genNodeStore: NodeStore
    
    def setup(in: T, litdesc: LitDesc[T]): in.type = {
      in.rebind(LitAssignSpell( LitNode(genNodeStore) ))
      in.setDescRef(litdesc, false)
      in.forceSetName(NameLit(litdesc), NameFromLit, false)
      in
    }
}

case class BoolLitMap(value: Boolean) extends LitMap[Bool] with LitMapElementImpl[Bool] {
  def genNodeStore = UBits(Some(1))
}
case class UIntLitMap(value: BigInt, width: Option[Int]) extends LitMap[UInt] with LitMapElementImpl[UInt] {
  def genNodeStore = UBits(width)
}
case class SIntLitMap(value: BigInt, width: Option[Int]) extends LitMap[SInt] with LitMapElementImpl[SInt] {
  def genNodeStore = SBits(width)
}
/*
case class Vec[D](???) extends ListMap[Vec[D]]
case class Vec(???) extends ListMap[HardwareTuple]
*/
