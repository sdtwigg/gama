package gama
package frontend

import implementation._

@annotation.implicitNotFound("Cannot create Mux for type ${D}")
trait Muxable[D<:Data] {
  def muxRetVal(tc: D, fc: D): D
  def mux(cond: Bool, tc: D, fc: D, info: EnclosureInfo): D = 
    MuxableImpl.mux(this, cond, tc, fc, info)
}
object Muxable {
  implicit def genBundleMuxable[B<:Bundle]: Muxable[B] = {
    new BundleMuxableImpl[B]
  }
}

object Mux {
  import scala.language.experimental.macros
  import implementation.macrodefs.{TransformMacro => XFORM}
  
  // External API
  def apply[RT<:Data](cond: Bool, tc: RT, fc: RT): RT = macro XFORM.doMux.xform[RT]

  // external->internal API
  def doMux[RT<:Data](cond: Bool, tc: RT, fc: RT, muxer: Muxable[RT], info: EnclosureInfo): RT = muxer.mux(cond, tc, fc, info)
}

