package gama
import internal._

object Reg {
  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}
  
  // External API
  def apply[D<:Data](model: D): D = macro XFORM.doNodeXFORM.xform[D]

  // external->internal API
  def doNodeXFORM[D<:Data](model: D, em: EnclosingModule): D = RegInternals(model, em)
}

