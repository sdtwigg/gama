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
  type Self <: LitMap[T] // So generalizeWith can use stored data
  def constructData: T // would be helpful for VecLitMap
  def setup(in: T, litdesc: LitDesc[T]): in.type
    // Vec uses this to ensure underlying types are the same
    // returns new version where stuff matches
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
    // TODO: this may be unnecessary if constructData is used...
    // although may want to put tighter restrictions on LitMap creation then...
    
    def setup(in: T, litdesc: LitDesc[T]): in.type = {
      in.rebind(LitAssignSpell( LitNode(genNodeStore) )) // also unnecessary with above TODO
      in.setDescRef(litdesc, false)
      in.forceSetName(NameLit(litdesc), NameFromLit, false)
      in
    }
}

trait LitMapVectorizable[D<:Data, LMT <: LitMap[D]] {
  def generalize(target: LMT, model: LMT): LMT // force target to mimic model but retain value
  def emptyData: D // used if VecLitMap.elemmaps is empty
}
// Allows that LitMap to be used to construct an Aggregate LitMap

case class BoolLitMap(value: Boolean) extends LitMap[Bool] with LitMapElementImpl[Bool] {
  type Self = BoolLitMap
  def constructData = Bool()
  def genNodeStore = UBits(Some(1))
}
object BoolLitMap {
  implicit object generalizer extends LitMapVectorizable[Bool, BoolLitMap] {
    def generalize(target: BoolLitMap, model: BoolLitMap) = target
    def emptyData = Bool()
  }
}

case class UIntLitMap(value: BigInt, width: Option[Int]) extends LitMap[UInt] with LitMapElementImpl[UInt] {
  type Self = UIntLitMap
  def constructData = UInt(width)
  def genNodeStore = UBits(width)
}
object UIntLitMap {
  implicit object generalizer extends LitMapVectorizable[UInt, UIntLitMap] {
    def generalize(target: UIntLitMap, model: UIntLitMap) =
      UIntLitMap(target.value, for(lw <- target.width; rw <- model.width) yield math.max(lw, rw))
    def emptyData = UInt()
  }
}

case class SIntLitMap(value: BigInt, width: Option[Int]) extends LitMap[SInt] with LitMapElementImpl[SInt] {
  type Self = SIntLitMap
  def constructData = SInt(width)
  def genNodeStore = SBits(width)
}
object SIntLitMap {
  implicit object generalizer extends LitMapVectorizable[SInt, SIntLitMap] {
    def generalize(target: SIntLitMap, model: SIntLitMap) =
      SIntLitMap(target.value, for(lw <- target.width; rw <- model.width) yield math.max(lw, rw))
    def emptyData = SInt()
  }
}

class VecLitMap[D<:Data: Muxable, LMT<:LitMap[D]]
  (initial_elemmaps: Seq[LMT with LitMap[D]], assume_head_generalized: Boolean)
  (implicit generalizer: LitMapVectorizable[D, LMT])
  extends LitMap[Vec[D]]
{
  // with LitMap[D] is to help scala type inferer figure out D
  type Self = VecLitMap[D, LMT]

  val length = initial_elemmaps.length
  protected val nongeneralized_tail = initial_elemmaps.tail
  lazy protected val generalized_head: Option[LMT] = initial_elemmaps.headOption.map(head => {
    if(assume_head_generalized) head
    else nongeneralized_tail.foldLeft(head)( (cand_head, elem) => generalizer.generalize(cand_head, elem) )
  }) // generalized_head is now capable of holding any element in the Vec
  lazy val elemmaps: Seq[LMT] = {
    generalized_head.map(head =>
      head +: (nongeneralized_tail map (elem => generalizer.generalize(elem, head)))
    ).getOrElse(initial_elemmaps)
  } // now, all elements in the Vec can hold any other element
  // Split so that, when doing Vec of Vec, do not prematurely generate other elements
  
  def constructData = Vec(length,
    elemmaps.headOption.map(_.constructData).getOrElse(generalizer.emptyData)
  )

  def setup(in: Vec[D], litdesc: LitDesc[Vec[D]]): in.type = {
    require(in.length == elemmaps.length, "Internal Error: Literal Vec.length != LitMaps.length")
    // Handle children (the Vec elements)
    (in.elements zip elemmaps).foreach({ case (elem, emap) => LitDesc(elem, emap) })

    // Handle self (the Vec), DO NOT PROPOGATE DESC OR NAME
    in.setDescRef(litdesc, false)
    in.forceSetName(NameLit(litdesc), NameFromLit, false)
    in
  }
}
object VecLitMap {
  def apply[D<:Data: Muxable, LMT<:LitMap[D]](initial_elemmaps: Seq[LMT with LitMap[D]])(implicit generalizer: LitMapVectorizable[D, LMT]): VecLitMap[D, LMT] =
    new VecLitMap(initial_elemmaps, false)

  def unapply(test: VecLitMap[_<:Data,_<:LitMap[_<:Data]]): Option[Seq[LitMap[_<:Data]]] = Some(test.elemmaps)
    // Long-term TODO: fixup this typing? losing the type may be bad...

  implicit def genGeneralizer[D<:Data: Muxable, LMT<:LitMap[D]]
    (implicit generalizer: LitMapVectorizable[D, LMT]): LitMapVectorizable[Vec[D],VecLitMap[D, LMT]] =
  {
    ??? // TODO
    // REMEMBER, A new VecLitMap will already match its internal elements
    // So, for a Vec of Vec, just take the head of each inner Vec, match them, and construct with
    //   generalized_head and nongeneralized_tail, other elements will be auto-regeneralized
    // This IS still slightly inefficient though, (TODO)
    //   As, after the inner Vecs are reconstructed, the head will be regeneralized, even though it is known that
    //   it is already general enough. (use the assume_head_generalized option)
  }
}

//case class ??? extends ListMap[HardwareTuple]

