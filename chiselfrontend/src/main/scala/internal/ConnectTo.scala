package gama
package internal

case class Sink[+D<:Data](data: D) extends AnyVal
case class Source[+D<:Data](data: D) extends AnyVal

@annotation.implicitNotFound("Cannot connect data of type ${From} to data of type ${To}. No implicit ConnectTo[${To},${From}] resolvable.")
trait ConnectTo[To<:Data, -From<:Data] {
  def connect(sink: Sink[To], source: Source[From], em: EnclosingModule): Unit
  // TODO: Should the contravariance exist?
  // + does prevent creating redundant ConnectTo
  //   e.g. ConnectTo[UInt, BaseElem] can take place of ConnectTo[UInt, UInt/SInt/Bool/...]
  // - that might be strange for hwtuple
}
object ConnectTo {
  def apply[To<:Data,From<:Data](implicit ev: ConnectTo[To, From]) = ev
  trait ConnectToImpl[To<:Data,From<:Data] extends ConnectTo[To,From] {
    def verifyConnect(sink: Sink[To], source: Source[From]): Unit
    def connect(sink: Sink[To], source: Source[From], em: EnclosingModule): Unit = {
      verifyConnect(sink, source)
      em.getActiveJournal.append(ConnectData(sink, source))
    }
  }
  implicit def genBundleConnectToBundle[B<:Bundle]: ConnectTo[B,Bundle] = new BundleConnectToBundleImpl[B]
}
// TODO: Global Data := Data assign added via implicit class that furthermore uses implicit ConnectTo?
// As of right now, must sort-of make sure ConnectTo typeclasses added along with new := so that Vec remains sane

class BundleConnectToBundleImpl[B<:Bundle] extends ConnectTo.ConnectToImpl[B, Bundle] {
  def verifyConnect(sink: Sink[B], source: Source[Bundle]): Unit = {
    Bundle.basicfunctionality.verifyConnect(sink, source)
  }
}
