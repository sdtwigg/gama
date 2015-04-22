package gama
package internal

// For mono-connect
case class Sink[+D<:Data](data: D) extends AnyVal
case class Source[+D<:Data](data: D) extends AnyVal

sealed trait ConnectDetails
case object ConnectAll extends ConnectDetails // Used for Element and Aggregates when all fields connect
case class  ConnectTuple(fields: Seq[Tuple2[String,ConnectDetails]]) extends ConnectDetails
case class  ConnectVec(elemdetails: ConnectDetails) extends ConnectDetails

// SINGLE DIRECTION (MONO) CONNECT
// TODO: Global Data := Data assign added via implicit class that furthermore uses implicit ConnectTo?
// As of right now, must sort-of make sure ConnectTo typeclasses added along with new := so that Vec remains sane
// TODO: Should the contravariance exist?
// + does prevent creating redundant ConnectTo
//   e.g. ConnectTo[UInt, BaseElem] can take place of ConnectTo[UInt, UInt/SInt/Bool/...]
// - that might be strange for hwtuple
@annotation.implicitNotFound("Cannot connect data of type ${From} to data of type ${To}. No implicit ConnectTo[${To},${From}] resolvable.")
trait ConnectTo[To<:Data, -From<:Data] {
  def monoDetails(sink: Sink[To], source: Source[From]): ConnectDetails
  def monoConnect(sink: Sink[To], source: Source[From], info: EnclosureInfo): Unit
}
object ConnectTo {
  def apply[To<:Data,From<:Data](implicit ev: ConnectTo[To, From]) = ev
  trait ConnectToImpl[To<:Data,From<:Data] extends ConnectTo[To,From] {
    def monoConnect(sink: Sink[To], source: Source[From], info: EnclosureInfo): Unit =
      info.em.getActiveJournal.append(ConnectData(sink, source, monoDetails(sink,source), info))
  } // should this just be ConnectTo?

  implicit def genBundleConnectToBundle[B<:Bundle]: ConnectTo[B,Bundle] = new BundleConnectToBundleImpl[B]{}
}

trait ConnectSelf[D<: Data] extends ConnectTo[D, D]

