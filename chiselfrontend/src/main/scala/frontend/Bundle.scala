package gama
package frontend

import implementation._

trait BundleReflection extends BundleReflectionImpl {self: HardwareTuple =>}

object Bundle {
  implicit object basicfunctionality
    extends BundleConnectToBundleImpl[Bundle]
    with BundleBiConnectBundleImpl[Bundle,Bundle]
  // Not strictly needed; however, effectively memoized for use in :=, <>
}
abstract class Bundle extends HardwareTuple with BundleReflection {
  import scala.language.experimental.macros
  import implementation.macrodefs.{TransformMacro => XFORM}

  // External API
  def :=(source: Bundle): Unit = macro XFORM.doConnectTo.sourcearg
  def <>(right:  Bundle): Unit = macro XFORM.doBiConnect.rightarg

  // external->internal API
  def doConnectTo(source: Bundle, info: EnclosureInfo): Unit =
    Bundle.basicfunctionality.monoConnect(Sink(this), Source(source), info)
  def doBiConnect(right: Bundle, info: EnclosureInfo): Unit =
    Bundle.basicfunctionality.biConnect(Left(this), Right(right), info)
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
trait BundleConnectToBundleImpl[B<:Bundle] extends ConnectTo[B, Bundle] {
  def monoDetails(sink: Sink[B], source: Source[Bundle], info: EnclosureInfo): ConnectDetails =
    UnsafeConnectToDataImpl.monoDetails(sink, source, info)
}
trait BundleBiConnectBundleImpl[LT<:Bundle,RT<:Bundle] extends BiConnect[LT, RT] {
  def biDetails(left: Left[LT], right: Right[RT], info: EnclosureInfo): BiConnectDetails =
    UnsafeConnectToDataImpl.biDetails(left, right, info)
}

case class NeedCopyMethodException(containerType: String)
  extends ChiselException(s"AnonBundle ${containerType} needs an explicit simplecopy method")
trait Anon {
  self: HardwareTuple with SimpleCopy =>

  def simplecopy: this.type = throw NeedCopyMethodException(this.getClass.getName)
}

