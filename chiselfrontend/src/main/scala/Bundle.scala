package gama
import internal._

trait BundleReflection extends BundleReflectionImpl {self: HardwareTuple =>}

object Bundle {
  implicit object basicfunctionality extends ConnectSelf.ConnectSelfImpl[Bundle] {
    def verifyConnectSelf(sink: Sink[Bundle], source: Source[Bundle]): Unit = {
      //TODO: actual checks here...
      // Likely will need to do some sort of matching
    }
  }
}
abstract class Bundle extends HardwareTuple with BundleReflection {
  def :=(source: Bundle)(implicit em: EnclosingModule) = ConnectSelf[Bundle].connectSelf(Sink(this), Source(source), em) 
}

case class ImproperBundleMuxException(tc: String, fc: String)
  extends ChiselException (s"For Mux, either tc($tc) or fc($fc) must directly descend from the other.")
class BundleSelfMuxableImpl[B<:Bundle] extends SelfMuxable[B] {
  def muxRetVal(tc: B, fc: B): B = {
    if(tc.getClass.isAssignableFrom(fc.getClass)) tc.copy else
    if(fc.getClass.isAssignableFrom(tc.getClass)) fc.copy else
      throw ImproperBundleMuxException(tc.getClass.getName, fc.getClass.getName)
  }
}

case class NeedCopyMethodException(containerType: String)
  extends ChiselException(s"AnonBundle ${containerType} needs an explicit copy method")
trait Anon {
  self: HardwareTuple =>

  def copy: this.type = throw NeedCopyMethodException(this.getClass.getName)
}
