package gama
package frontend
package implementation

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
  protected[gama] def preciseMonoConnect(sink: Sink[To], source: Source[From], info: EnclosureInfo,
    targetloc: Tuple2[journal.Journal, journal.Entry]): Unit =
  {
    targetloc._1.insertAfter(journal.ConnectData(sink, source, monoDetails(sink,source), info), targetloc._2)
  }
    // Use for in the VERY RARE cases that need to override the journal and entry position written to
    //   e.g. Module uses this to ensure the reset for a child is connected in precisely the right place
    //        regardless of usercode insanity (likely, abuse of lazy val or def in that child module)
    // USE WITH EXTREME CAUTION as it is not ensured journal is even on the EnclosingModule's journal stack
}
object ConnectTo {
  type From[D<:Data] = {type CB[To<:Data] = ConnectTo[To, D]}
  // context bound of form D<:Data: ConnectTo.From[UInt]#CB means
  //   supplied D must be able to connect from a UInt

  type Self = {type CB[D<:Data] = ConnectTo[D, D]}
  // context bound of form D<:Data: ConnectTo.Self#CB means
  //   supplied D must be able to connect from D (itself)

  def apply[To<:Data,From<:Data](implicit ev: ConnectTo[To, From]) = ev
  trait ConnectToImpl[To<:Data,From<:Data] extends ConnectTo[To,From] {
    def monoConnect(sink: Sink[To], source: Source[From], info: EnclosureInfo): Unit =
      info.em.getActiveJournal.append(journal.ConnectData(sink, source, monoDetails(sink,source), info))
  } // should this just be ConnectTo?

  implicit def genBundleConnectToBundle[B<:Bundle]: ConnectTo[B,Bundle] = new BundleConnectToBundleImpl[B]{}
}

