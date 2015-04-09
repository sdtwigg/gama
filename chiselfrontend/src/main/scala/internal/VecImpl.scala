package gama
import internal._
import scala.collection.{immutable=>immutable}

@annotation.implicitNotFound("""Cannot create Vec of elements with common type ${D}.
Most common reason is that no self-muxing operation (SelfMuxable[${D}]) available""")
trait Vectorizable[D<:Data] { def muxer: SelfMuxable[D] }
// Only reason Vectorizable trait exists is so that failing to create a Vec gives a specialized error message
object Vectorizable {
  implicit def vectorizer[D<:Data: SelfMuxable]: Vectorizable[D] = new Vectorizable[D] {val muxer = implicitly[SelfMuxable[D]]}
}

abstract class VecImpl[D<:Data: Vectorizable](initialModel: D) {
  self: Vec[D] =>
  // D must be invariant because of assignment (:=), amongst other reasons
  // TODO: INDEXED SEQ MAY BE SUBTLY INCORRECT
  
  private[this] val mutableElemType: D = initialModel.copy
  def elemType: D = mutableElemType.copy
  protected[gama] val elements: immutable.IndexedSeq[D] = Vector.fill(length)(elemType)

  protected[gama] def rebind(xform: NodeSpell[_<:Node]): this.type = {
    mutableElemType.rebind(xform)
    elements.foreach(elem => elem.rebind(xform))
    // TODO: VERIFY ALL VEC ELEMENTS STILL IDENTICAL to elemType
    // Should be sufficient to check NodeStorage equality and perhaps node type
    this
  }

  def nodes = elements.flatMap(_.nodes)

  implicit protected val eltmuxer: SelfMuxable[D] = implicitly[Vectorizable[D]].muxer
  def :=(source: Vec[D])(implicit eltxfer: ConnectSelf[D], em: EnclosingModule) = ConnectSelf[Vec[D]].connectSelf(Sink(this), Source(source), em)

  // Until Synthesized, elemType (clones) 'hide' all access to elements (see lookup)
  def lookup(index: Int): D = {
    // TODO: OBSCURATION MAY BE UNNECESSARY AND WRONG
    nodes.headOption.getOrElse(mutableElemType) match {
      case _: SPEC => {
        if(index>length) throw new java.lang.IndexOutOfBoundsException(index.toString)
        elemType
      }
      case _: Synthesizable => (elements(index))
    }
  }
  def apply(index: Int): D = lookup(index)

  def lookupIsConnectable(selector: UInt): Boolean = {
    nodes.headOption.getOrElse(mutableElemType) match {
      case _: SPEC => {throw ExpectedNodeException("Synthesizable","SPEC")}
      case _: Connectable => {NodeCheck.assertConnectable(this); true}
      case _: NonConnectable => {NodeCheck.assertNonConnectable(this); false}
    }
  }
  
  def propogateName(newname: NameTree, newsource: NameSource): Unit = {
    elements.zipWithIndex.foreach({case (elem: Data, idx: Int) => 
      elem.forceSetName(NameIndex(newname, idx), newsource, true)
    })
  }
  def propogateDescRef(newdesc: Desc): Unit = {
    elements.foreach( elem => {elem.setDescRef(newdesc, true)} )
  }
}

trait VecObjectImpl {
  def apply[D<:Data: Vectorizable](size: Int, model: D): Vec[D] = new Vec(size, model)
//  def apply[D<:Data: Vectorizable](elts: D*)(implicit em: EnclosingModule) = new Vec(elts.toVector)
//  def apply[D<:Data: Vectorizable](elts: immutable.Seq[D])(implicit em: EnclosingModule) = new Vec(elts)
  // mutable.Seq[D] can still be used but must be unpacked into the first construction e.g. Vec(Array(...):_*)
  // Second constructor just prevents unnecessary unpacking and repacking for case of immutable.Seq[D]

  // Basically, Vec can only be regenerated, transfered, etc. if constituent elements are regeneratable or transferable
  implicit def selfMuxer[D<:Data: SelfMuxable]: SelfMuxable[Vec[D]] = new SelfMuxable[Vec[D]] {
    def muxRetVal(tc: Vec[D], fc: Vec[D]) = {
      require(tc.elements.length==fc.elements.length, "Cannot mux together two vectors of different length")
      Vec(tc.elements.length, implicitly[SelfMuxable[D]].muxRetVal(tc.elemType, fc.elemType))
    }
  }
  implicit def connectSelf[D<:Data: ConnectSelf]: ConnectSelf[Vec[D]] = new ConnectSelf.ConnectSelfImpl[Vec[D]] {
    def verifyConnectSelf(sink: Sink[Vec[D]], source: Source[Vec[D]]) = {
      require(source.data.elements.length==sink.data.elements.length, "Cannot assign to/from two vectors of different length")
      // TODO: VERIFY SUBELEMENTS CONNECTION?
      // := already grabs the associated element-level ConnectSelf
    }
  }
}
