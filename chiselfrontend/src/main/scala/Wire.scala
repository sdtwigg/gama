package gama
import internal._

object Wire {
  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}
  
  // External API
  def apply[D<:Data](model: D): D = macro XFORM.doNodeXFORM.xform[D]

  // external->internal API
  def doNodeXFORM[D<:Data](model: D, info: EnclosureInfo): D = WireInternals(model, info)
}

