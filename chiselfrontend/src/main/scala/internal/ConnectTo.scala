package gama
package internal

case class Sink[+D<:Data](data: D) extends AnyVal
case class Source[+D<:Data](data: D) extends AnyVal

case class Left[+D<:Data](data: D) extends AnyVal
case class Right[+D<:Data](data: D) extends AnyVal

// TODO: Global Data := Data assign added via implicit class that furthermore uses implicit ConnectTo?
// As of right now, must sort-of make sure ConnectTo typeclasses added along with new := so that Vec remains sane
// TODO: Should the contravariance exist?
// + does prevent creating redundant ConnectTo
//   e.g. ConnectTo[UInt, BaseElem] can take place of ConnectTo[UInt, UInt/SInt/Bool/...]
// - that might be strange for hwtuple
// TODO: Should From be From <: Data >: To
// + means the biConnect is slightly more sensible
// - might be unnecessarily restrictive
// -- could split BiConnect into own trait? but that may be too verbose
@annotation.implicitNotFound("Cannot connect data of type ${From} to data of type ${To}. No implicit ConnectTo[${To},${From}] resolvable.")
trait ConnectTo[To<:Data, -From<:Data] {
  def monoDetails(sink: Sink[To], source: Source[From]): ConnectDetails
  def monoConnect(sink: Sink[To], source: Source[From], em: EnclosingModule): Unit
  
  def biDetails(left: Left[To], right: Right[To]): BiConnectDetails
  def biConnect(left: Left[To], right: Right[To], em: EnclosingModule): Unit
  // Should be To <-> To since could be either way
}
object ConnectTo {
  def apply[To<:Data,From<:Data](implicit ev: ConnectTo[To, From]) = ev
  trait ConnectToImpl[To<:Data,From<:Data] extends ConnectTo[To,From] {
    def monoConnect(sink: Sink[To], source: Source[From], em: EnclosingModule): Unit =
      em.getActiveJournal.append(ConnectData(sink, source, monoDetails(sink,source)))

    def biConnect(left: Left[To], right: Right[To], em: EnclosingModule): Unit =
      ???//em.getActiveJournal.append(ConnectData(sink, source, monoDetails(sink,source)))
  } // should this just be ConnectTo?

  implicit def genBundleConnectToBundle[B<:Bundle]: ConnectTo[B,Bundle] = new BundleConnectToBundleImpl[B]
}

trait ConnectSelf[D<: Data] extends ConnectTo[D, D]

// NEVER PUT THIS IN AN IMPLICIT RESOLUTION PATH
// Only meant to be used by Bundle ConnectTo
case class RuntimeMisconnectException(sink: String, source: String)
  extends ChiselException(s"In a non-typechecked connect, improper attempted connection: tried to connect a $source to a $sink.")
object UnsafeConnectToDataImpl extends ConnectTo.ConnectToImpl[Data, Data] {
  def monoDetails(sink: Sink[Data], source: Source[Data]): ConnectDetails = {
    sink.data match {
      case left_e: Element => (source.data match {
        case right_e: Element => (ConnectAll)
        case _ => throw RuntimeMisconnectException(sink.data.getClass.getName,source.data.getClass.getName)
      }) // TODO: VERIFY ELEMENT CONNECTION SANE?
      case left_v: Vec[Data @unchecked] => (source.data match {
        case right_v: Vec[Data @unchecked] =>
          (Vec.connectTo[Data,Data](this).monoDetails(Sink(left_v),Source(right_v)))
        case _ => throw RuntimeMisconnectException(sink.data.getClass.getName,source.data.getClass.getName)
      })
      case left_t: HardwareTuple => (source.data match {
        case right_t: HardwareTuple => (monoTuple(left_t, right_t)) // in own function since so complicated
        case _ => throw RuntimeMisconnectException(sink.data.getClass.getName,source.data.getClass.getName)
      })
    }
  }
  def monoTuple(sink: HardwareTuple, source: HardwareTuple): ConnectDetails = {
    val candidates = sink.subfields_ordered
    val connect_list: Seq[Tuple2[String,ConnectDetails]] = candidates.flatMap({case (field, sink_elem) => {
      val source_elem_opt = source.subfields.get(field)
      val details = source_elem_opt.map(source_elem =>
        try { monoDetails(Sink(sink_elem), Source(source_elem)) }
        catch { case e: ChiselException => {throw TraversalException(field, sink.getClass.getName, e)} }
      )
      details.map((field, _))
    }})
    if( (connect_list.length == sink.subfields_ordered.length) &&
        (connect_list.length == source.subfields.size) &&
        connect_list.forall(_._2 == ConnectAll)
    ) { ConnectAll }
    else { ConnectTuple(connect_list) }
  }
  
  def biDetails(left: Left[Data], right: Right[Data]): BiConnectDetails = ???
}
