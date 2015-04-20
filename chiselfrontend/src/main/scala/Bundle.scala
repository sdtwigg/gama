package gama
import internal._

trait BundleReflection extends BundleReflectionImpl {self: HardwareTuple =>}

object Bundle {
  implicit object basicfunctionality
    extends BundleConnectToBundleImpl[Bundle]
    with BundleBiConnectBundleImpl[Bundle,Bundle]
  // Note strictly needed; however, effectively memoized for use in :=, <>
}
abstract class Bundle extends HardwareTuple with BundleReflection {
  def :=(source: Bundle)(implicit em: EnclosingModule) =
    Bundle.basicfunctionality.monoConnect(Sink(this), Source(source), em) 
  def <>(right: Bundle)(implicit em: EnclosingModule) =
    Bundle.basicfunctionality.biConnect(Left(this), Right(right), em) 
}

case class ImproperBundleMuxException(tc: String, fc: String)
  extends ChiselException (s"For Mux, either tc($tc) or fc($fc) must directly descend from the other.")

// HELPER TRAIT FOR MAKING MUXABLE BUNDLES
class BundleMuxableImpl[B<:Bundle] extends Muxable[B] {
  def muxRetVal(tc: B, fc: B): B = {
    if(tc.getClass.isAssignableFrom(fc.getClass)) tc.copy else
    if(fc.getClass.isAssignableFrom(tc.getClass)) fc.copy else
      throw ImproperBundleMuxException(tc.getClass.getName, fc.getClass.getName)
  }
}
// HELPER TRAITS FOR CONNECTING A BUNDLE TO ANY OTHER BUNDLE
trait BundleConnectToBundleImpl[B<:Bundle] extends ConnectTo.ConnectToImpl[B, Bundle] {
  def monoDetails(sink: Sink[B], source: Source[Bundle]): ConnectDetails =
    UnsafeConnectToDataImpl.monoDetails(sink, source)
}
trait BundleBiConnectBundleImpl[LT<:Bundle,RT<:Bundle] extends BiConnect.BiConnectImpl[LT, RT] {
  def biDetails(left: Left[LT], right: Right[RT], em: EnclosingModule): BiConnectDetails =
    UnsafeConnectToDataImpl.biDetails(left, right, em)
}

case class NeedCopyMethodException(containerType: String)
  extends ChiselException(s"AnonBundle ${containerType} needs an explicit simplecopy method")
trait Anon {
  self: HardwareTuple with SimpleCopy =>

  def simplecopy: this.type = throw NeedCopyMethodException(this.getClass.getName)
}

