package gama
import internal._

trait BundleReflection extends BundleReflectionImpl {self: HardwareTuple =>}

object Bundle {
  //implicit object basicfunctionality extends BundleConnectToBundleImpl[Bundle]
  // This never actually gets used....
}
abstract class Bundle extends HardwareTuple with BundleReflection {
  def :=(source: Bundle)(implicit em: EnclosingModule) =
    ConnectTo[Bundle,Bundle].connect(Sink(this), Source(source), em) 
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
// HELPER TRAIT FOR CONNECTING A BUNDLE TO ANY OTHER BUNDLE
class BundleConnectToBundleImpl[B<:Bundle] extends ConnectTo.ConnectToImpl[B, Bundle] {
  def calcDetails(sink: Sink[B], source: Source[Bundle]): ConnectDetails =
    UnsafeConnectToDataImpl.calcDetails(sink, source)
}

case class NeedCopyMethodException(containerType: String)
  extends ChiselException(s"AnonBundle ${containerType} needs an explicit simplecopy method")
trait Anon {
  self: HardwareTuple with SimpleCopy =>

  def simplecopy: this.type = throw NeedCopyMethodException(this.getClass.getName)
}

