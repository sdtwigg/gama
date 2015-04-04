package gama
import internal._
import scala.collection.{immutable=>immutable}

@annotation.implicitNotFound("""Cannot create Vec of elements with common type ${D}.
Most common reason is that no self-muxing operation (SelfMuxable[${D}]) available""")
trait Vectorizable[D<:Data] { val muxer: SelfMuxable[D] }
// Only reason Vectorizable trait exists is so that failing to create a Vec gives a specialized error message
object Vectorizable {
  implicit def vectorizer[D<:Data: SelfMuxable]: Vectorizable[D] = new Vectorizable[D] {val muxer = implicitly[SelfMuxable[D]]}
}

class Vec[D<:Data: Vectorizable](val size: Int, initialModel: D) extends Aggregate with Accessible[D] {
  val elemType: D = initialModel.copy
  private val elements: immutable.Seq[D] = Vector.fill(size)(elemType.copy)

  protected[gama] def rebind(xform: NodeSpell[_<:Synthesizable], em: EnclosingModule): this.type = {
    elemType.rebind(xform, em)
    elements.foreach(elem => elem.rebind(xform, em))
    // TODO: VERIFY ALL VEC ELEMENTS STILL IDENTICAL to elemType
    this
  }

  def nodes = elements flatMap(_.nodes)

  implicit protected val eltmuxer: SelfMuxable[D] = implicitly[Vectorizable[D]].muxer
  def :=(source: Vec[D])(implicit eltxfer: SelfTransfer[D], em: EnclosingModule) = implicitly[SelfTransfer[Vec[D]]].selfTransfer(source, this, em)
  def copy = Vec(size, elemType).asInstanceOf[this.type]

  // don't allow individual element access until synthesized
  def lookup(index: Int): D = {
    OpCheck.assertSynthesizable(this)
    elements(index)
  }
  def apply(index: Int): D = lookup(index)

  def lookupCheck(selector: UInt): Unit = {OpCheck.assertSynthesizable(this)}

  
  def propogateName(): Unit = {
    elements.zipWithIndex.foreach({case (elem: Data, idx: Int) => 
      elem.name = (s"${name.get}(${idx})", namePriority.get)
    })
  }
  def propogateDescRef(): Unit = {
    elements.foreach(elem => {elem.descRef = descRef.get})
  }
}
object Vec {
  def apply[D<:Data: Vectorizable](size: Int, model: D): Vec[D] = new Vec(size, model)
//  def apply[D<:Data: Vectorizable](elts: D*)(implicit em: EnclosingModule) = new Vec(elts.toVector)
//  def apply[D<:Data: Vectorizable](elts: immutable.Seq[D])(implicit em: EnclosingModule) = new Vec(elts)
  // mutable.Seq[D] can still be used but must be unpacked into the first construction e.g. Vec(Array(...):_*)
  // Second constructor just prevents unnecessary unpacking and repacking for case of immutable.Seq[D]

  // Basically, Vec can only be regenerated, transfered, etc. if constituent elements are regeneratable or transferable
  implicit def selfMuxer[D<:Data]: SelfMuxable[Vec[D]] = new SelfMuxable[Vec[D]] {
    def verifyMux(cond: Bool, tc: Vec[D], fc: Vec[D]) = {
      require(tc.elements.length==fc.elements.length, "Cannot mux together two vectors of different length")
    }
  }
  implicit def selfTransfer[D<:Data: SelfTransfer]: SelfTransfer[Vec[D]] = new SelfTransfer.SelfTransferImpl[Vec[D]] {
    def verifyTransfer(source: Vec[D], sink: Vec[D]) = {
      require(source.elements.length==sink.elements.length, "Cannot assign to/from two vectors of different length")
    }
  }
}

