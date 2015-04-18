package gama
import internal._
import scala.collection.{immutable=>immutable}

@annotation.implicitNotFound("""Cannot create Vec of elements with common type ${D}.
Most common reason is that no self-muxing operation (Muxable[${D}]) available""")
trait Vectorizable[D<:Data] { def muxer: Muxable[D] }
// Only reason Vectorizable trait exists is so that failing to create a Vec gives a specialized error message
object Vectorizable {
  implicit def vectorizer[D<:Data: Muxable]: Vectorizable[D] = new Vectorizable[D] {val muxer = implicitly[Muxable[D]]}
} // TODO: REMOVE DEPENDENCY ON SELFMUXABLE?

abstract class VecImpl[D<:Data: Vectorizable](initialModel: D) {
  self: Vec[D] =>
  // D must be invariant because of assignment (:=), amongst other reasons
  // TODO: INDEXED SEQ MAY BE SUBTLY INCORRECT
  
  protected[gama] val elemType: D = initialModel.copy
  // TODO: BETTER DEFINE WHAT elemType is
  protected[gama] val elements: immutable.IndexedSeq[D] = Vector.fill(length)(elemType.copy)

  private def enforceElementConsistency(): Unit = {
    elements.foreach(elem => elem.mimic(elemType, asSPEC=false))
      // false b/c exact mimic required: respective nodes of elements should all be completely identical
  }
  protected[gama] def rebind(xform: NodeSpell[_<:Node]): this.type = {
    elemType.rebind(xform)
    enforceElementConsistency()
    this
  }
  protected[gama] def mimic(model: Data, asSPEC: Boolean): Unit = {
    model match {
      case v: Vec[_] => {
        elemType.mimic(v.elemType, asSPEC)
        enforceElementConsistency()
      }
      case _ => throw StructuralMimicException
    }
  }

  def nodes = elements.flatMap(_.nodes)

  implicit protected val eltmuxer: Muxable[D] = implicitly[Vectorizable[D]].muxer
  def :=[From<:Data](source: Vec[From])(implicit em: EnclosingModule, writer: ConnectTo[Vec[D],Vec[From]]) = writer.connect(Sink(this), Source(source), em)

  def lookup(index: Int): D = elements(index)
  def apply(index: Int): D = lookup(index)

  def lookupIsConnectable(selector: UIntLike): Boolean = {
    nodes.headOption.getOrElse(elemType) match {
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
  implicit def selfMuxer[D<:Data: Muxable]: Muxable[Vec[D]] = new Muxable[Vec[D]] {
    def muxRetVal(tc: Vec[D], fc: Vec[D]) = {
      require(tc.elements.length==fc.elements.length, "Cannot mux together two vectors of different length")
      Vec(tc.elements.length, implicitly[Muxable[D]].muxRetVal(tc.elemType, fc.elemType))
    }
  }
  implicit def connectTo[To<:Data,From<:Data](implicit eltconnect: ConnectTo[To,From]): ConnectTo[Vec[To],Vec[From]] = new ConnectTo.ConnectToImpl[Vec[To],Vec[From]] {
    def verifyConnect(sink: Sink[Vec[To]], source: Source[Vec[From]]) = {
      require(source.data.elements.length==sink.data.elements.length, "Cannot assign to/from two vectors of different length")
      // TODO: VERIFY SUBELEMENTS CONNECTION?
      // := already grabs the associated element-level ConnectSelf
    }
  }
}

