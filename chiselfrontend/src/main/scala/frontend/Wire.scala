package gama
package frontend

import implementation._
  
// Activate Scala Language Features
import scala.language.experimental.macros
import implementation.macrodefs.{TransformMacro => XFORM}

object Wire {
  // External API
  def apply[D<:Data](model: D): D = macro XFORM.doNodeXFORM.xform[D]

  // external->internal API
  def doNodeXFORM[D<:Data](model: D, info: EnclosureInfo): D = WireInternals(model, info)
}

