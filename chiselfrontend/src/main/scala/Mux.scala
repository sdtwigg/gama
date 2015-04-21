package gama
import internal._

object Mux {
  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}
  
  // External API
  def apply[RT<:Data](cond: Bool, tc: RT, fc: RT): RT = macro XFORM.doMux.xform[RT]

  // external->internal API
  def doMux[RT<:Data](cond: Bool, tc: RT, fc: RT, muxer: Muxable[RT], em: EnclosingModule): RT = muxer.mux(cond, tc, fc, em)
}

