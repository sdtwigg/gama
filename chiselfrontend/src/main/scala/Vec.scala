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

class Vec[D<:Data: Vectorizable](val elements: immutable.Seq[D])(implicit em: EnclosingModule)  extends Aggregate {
  protected[gama] def rebind(xform: NodeSpell[_<:Synthesizable])(implicit em: EnclosingModule): this.type = {
    elements.foreach(elem => elem.rebind(xform))
    this
  }

  implicit protected val eltmuxer: SelfMuxable[D] = implicitly[Vectorizable[D]].muxer
  def :=(source: Vec[D])(implicit eltxfer: SelfTransfer[D]) = implicitly[SelfTransfer[Vec[D]]].selfTransfer(source, this)
  def copy = new Vec(elements.map(_.copy)).asInstanceOf[this.type]
}
object Vec {
  def apply[D<:Data: SelfMuxable](elts: D*)(implicit em: EnclosingModule) = new Vec(elts.toVector)
  def apply[D<:Data: SelfMuxable](elts: immutable.Seq[D])(implicit em: EnclosingModule) = new Vec(elts)
  // mutable.Seq[D] can still be used but must be unpacked into the first construction e.g. Vec(Array(...):_*)
  // Second constructor just prevents unnecessary unpacking and repacking for case of immutable.Seq[D]

  // Basically, Vec can only be regenerated, transfered, etc. if constituent elements are regeneratable or transferable
  implicit def selfMuxer[D<:Data]: SelfMuxable[Vec[D]] = new SelfMuxable[Vec[D]] {
    // Don't need : SelfMuxable for D since all Vec[D] already will have one for use upon construction!
    def mux(cond: Bool, tc: Vec[D], fc: Vec[D])(implicit em: EnclosingModule) = {
      require(tc.elements.length==fc.elements.length, "Cannot mux together two vectors of different length")
      implicit val eltmuxer = tc.eltmuxer

      val muxpackage = tc.elements.zip(fc.elements)
      new Vec( muxpackage.map({case (tci, fci) => eltmuxer.mux(cond, tci, fci)}) )
    }
  }
  implicit def selfTransfer[D<:Data: SelfTransfer]: SelfTransfer[Vec[D]] = new SelfTransfer[Vec[D]] {
    def selfTransfer(source: Vec[D], sink: Vec[D])(implicit em: EnclosingModule) = {
      require(source.elements.length==sink.elements.length, "Cannot assign to/from two vectors of different length")
      
      val xferpackage = source.elements.zip(sink.elements)
      xferpackage.foreach( {case (srci, desti) => implicitly[SelfTransfer[D]].selfTransfer(srci, desti)} )
        // This may not even be necessary as IR can emit a bulk vector assign

      sink
    }
  }
}

