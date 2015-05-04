package gama
package frontend
package implementation

// A thin wrapper around the LitMap workhorse, really just a simple tag
trait LitDescImpl[T<:Data] {
  self: LitDesc[T] => 
    def validateRetVal(): Unit = NodeCheck.assertLitNode(retVal)

    def doWork(): Unit = {} //litMap.bind(retVal, this) done in manifest now
}

// THIS CANNOT BE TOTALLY FINAL SINCE USER MAY EXTEND
// This trait actually holds the literal (hah) information for LitDesc so that
//   it can map details from whatever T is to its constituent elements

sealed abstract class LitMap[T<:Data] {
  def constructData: T // build the Data T that can hold the literal
  def manifest: T = transform(constructData) // Main invocation // TODO: Be a lazy val???
  def asLitTree: LitTree
  
  protected[this] def bind(in: T): Unit
    // bind LitNode to elements, transform sub-elements with other litmaps
    //   called before a desc/name is set on the T by transform
  def transform(in: T): in.type = {
    bind(in)
    
    val litdesc = LitDesc(in, this)
    in.setDescRef(litdesc, false)
    in.forceSetName(NameLit(litdesc), NameFromLit, false)
    // Recall that each layer of a 'nested' literal has own name and LitDesc
    //   so do not bother propogating names and desc
    in
  }
}
// eg. to map to a vector, may take a Seq of LitMaps to the constituent elements
protected[gama] object LitMap {
  type Vectorizable[D<:Data] = {type CB[T<:LitMap[D]] = LitMapVectorizer[D, T]}
  // Partial application of LitMapVectorizer (although need #CB)
  // LitMap.Vectorizable[UInt]#CB[UIntLitMap] =:= LitMapVectorizer[D,UIntLitMap]
}

trait LitMapElementImpl[T<:Element] {
  self: LitMap[T] =>
    protected[this] def bind(in: T): Unit = {
      in.rebind(NodeAssignSpell( LitNode(constructData.node.storage) ))
        // TODO: could just trust that in is OKAY
        //   as if constructed with constructData, it will be
    }
}

// Allows that LitMap to be used to construct an Aggregate LitMap
trait LitMapVectorizer[D<:Data, LMT <: LitMap[D]] {
  def emptyData: D // used if VecLitMap.elemmaps is empty
  def generalize(target: LMT, model: LMT): LMT // force target to mimic model but retain value
    // Vec uses this to ensure underlying types are the same
    // returns new version where stuff matches
}

case class BoolLitMap(value: Boolean) extends LitMap[Bool] with LitMapElementImpl[Bool] {
  def constructData = Bool()
  def asLitTree = LitRawBits(if(value) 1 else 0, 1, false)
}
object BoolLitMap {
  implicit object generalizer extends LitMapVectorizer[Bool, BoolLitMap] {
    def emptyData = Bool()
    def generalize(target: BoolLitMap, model: BoolLitMap) = target
  }
}

case class UIntLitMap(value: BigInt, width: Int) extends LitMap[UInt] with LitMapElementImpl[UInt] {
  require( value>=0, "UInt Literal: value must be non-negative." )
  require( value.bitLength <= width, s"UInt Literal: width $width too small to hold $value (need ${value.bitLength})")

  def constructData = UInt(width)
  def asLitTree = LitRawBits(value, width, false)
}
object UIntLitMap {
  def apply(value: BigInt): UIntLitMap = UIntLitMap(value, math.max(value.bitLength,1))
    // TODO: at least 1 so that Literal 0 always has some width, is this ok?
  implicit object generalizer extends LitMapVectorizer[UInt, UIntLitMap] {
    def emptyData = UInt(Some(0))
    def generalize(target: UIntLitMap, model: UIntLitMap) =
      UIntLitMap(target.value, math.max(target.width, model.width))
  }
}

case class SIntLitMap(value: BigInt, width: Int) extends LitMap[SInt] with LitMapElementImpl[SInt] {
  require( value.bitLength+1 <= width,
    s"SInt Literal: width $width too small to hold $value (need ${value.bitLength+1})")

  def constructData = SInt(width)
  def asLitTree = LitRawBits(value, width, true)
}
object SIntLitMap {
  def apply(value: BigInt): SIntLitMap = SIntLitMap(value, value.bitLength+1)
  implicit object generalizer extends LitMapVectorizer[SInt, SIntLitMap] {
    def emptyData = SInt(Some(0))
    def generalize(target: SIntLitMap, model: SIntLitMap) = 
      SIntLitMap(target.value, math.max(target.width, model.width))
  }
}

// TODO: Maybe make this a builder and have the generalized one be a full case class
class VecLitMap[D<:Data: Muxable, LMT<:LitMap[D]: LitMap.Vectorizable[D]#CB] private
  (initial_elemmaps: Seq[LMT with LitMap[D]], assume_head_generalized: Boolean) extends LitMap[Vec[D]]
  // Note: 'with LitMap[D]' is to help scala type inferer figure out D
  // Also 'LitMap.Vectorizable[D]#CB' gets [LMT] applies
  //   and thus becomes: implicit ...: LitMapVectorizer[D, LMT])
{
  private[this] val elemgeneralizer = implicitly[LitMapVectorizer[D,LMT]] // need elem generalizer

  val length = initial_elemmaps.length
  protected val nongeneralized_tail = initial_elemmaps.drop(1) // tail excepts if list empty
  lazy protected val generalized_head: Option[LMT] = initial_elemmaps.headOption.map(head => {
    if(assume_head_generalized) head
    else nongeneralized_tail.foldLeft(head)( (cand_head, elem) => elemgeneralizer.generalize(cand_head, elem) )
  }) // generalized_head is now capable of holding any element in the Vec
  lazy val elemmaps: Seq[LMT] = {
    generalized_head.map(head =>
      head +: (nongeneralized_tail map (elem => elemgeneralizer.generalize(elem, head)))
    ).getOrElse(initial_elemmaps)
  } // now, all elements in the Vec can hold any other element
  // Split so that, when doing Vec of Vec, do not prematurely generate other elements
  
  def constructData = Vec(length,
    generalized_head.map(_.constructData).getOrElse(elemgeneralizer.emptyData)
  )
  def asLitTree = LitVec(elemmaps.map(_.asLitTree).toVector)

  protected[this] def bind(in: Vec[D]): Unit = {
    require(in.length == elemmaps.length, "Internal Error: Literal Vec.length != LitMaps.length")
    // Handle children (the Vec elements)
    (in.elements zip elemmaps).foreach({ case (elem, emap) => emap.transform(elem) })
  }
}

object VecLitMap {
  def apply[D<:Data: Muxable, LMT<:LitMap[D]: LitMap.Vectorizable[D]#CB](initial_elemmaps: Seq[LMT with LitMap[D]]) =
    new VecLitMap(initial_elemmaps, false)

  def unapply(test: VecLitMap[_<:Data,_<:LitMap[_<:Data]]): Option[Seq[LitMap[_<:Data]]] = Some(test.elemmaps)
    // Long-term TODO: fixup this typing? losing the type may be bad..., see other todo with builder vs case class

  implicit def genGeneralizer[D<:Data: Muxable, LMT<:LitMap[D]]
    (implicit elemgen: LitMapVectorizer[D, LMT]): LitMapVectorizer[Vec[D],VecLitMap[D, LMT]] =
      new LitMapVectorizer[Vec[D],VecLitMap[D, LMT]] 
  {
    def emptyData: Vec[D] = Vec(0, elemgen.emptyData)
    def generalize(target: VecLitMap[D, LMT], model: VecLitMap[D,LMT]): VecLitMap[D,LMT] = {
      require(target.length == model.length, "Internal Error: target.length must equal model.length")
      (for{
        target_head <- target.generalized_head
        model_head  <- model.generalized_head
      } yield {
        val general_target_head = elemgen.generalize(target_head, model_head)
        new VecLitMap(general_target_head +: (target.nongeneralized_tail), true)
          // override since know target_head was already generalized to tail
      }).getOrElse(target)
    }
    // REMEMBER, A new VecLitMap will already match its internal elements
    // So, for a Vec of Vec, just take the head of each inner Vec, match them, and construct with
    //   generalized_head and nongeneralized_tail, other elements will be auto-regeneralized
  }

}

//case class ??? extends ListMap[HardwareTuple]

