package gama
package frontend

import implementation._

object Reg {
  import scala.language.experimental.macros
  import implementation.macrodefs.{TransformMacro => XFORM}
  
  // External API
  def apply[D<:Data](model: D): D = macro XFORM.doNodeXFORM.xform[D]
  def apply[D<:Data](model: RModel[D]): D = macro XFORM.doNodeXFORM.xform[D]
  def apply[D<:Data](init:  RInit[D]): D = macro XFORM.doNodeXFORM.xinit[D]
  def apply[D<:Data](init:  RInitModel[D]): D = macro XFORM.doNodeXFORM.xinit[D]
  def apply[D<:Data](model: D, init: RInit[D]): D = macro XFORM.doNodeXFORM.xmodel_init[D]
  def apply[D<:Data](model: RModel[D], init: RInit[D]): D = macro XFORM.doNodeXFORM.xmodel_init[D]

  // external->internal API
  def doNodeXFORM[D<:Data](model: D, info: EnclosureInfo): D =         RegInternals(model, None, info)
  def doNodeXFORM[D<:Data](model: RModel[D], info: EnclosureInfo): D = RegInternals(model.data, None, info)
  def doNodeXFORM[D<:Data](init:  RInit[D], info: EnclosureInfo): D  = RegInternals(genericize(init.rval), handleInit(init, info), info)
  def doNodeXFORM[D<:Data](init:  RInitModel[D], info: EnclosureInfo): D = RegInternals(init.rval, handleInit(init, info), info)
  def doNodeXFORM[D<:Data](model: D, init: RInit[D], info: EnclosureInfo): D = RegInternals(model, handleInit(init, info), info)
  def doNodeXFORM[D<:Data](model: RModel[D], init: RInit[D], info: EnclosureInfo): D = RegInternals(model.data, handleInit(init, info), info)

  private[this] def genericize[D<:Data](in: D): D = in.copy.rebind(GenericizeSpell)
  private[this] def handleInit[D<:Data](init: RInitBase[D], info: EnclosureInfo): Some[Tuple2[Bool, D]] = init.ren match {
    case Some(reset) => Some((reset, init.rval))
    case None        => Some((info.em.reset, init.rval))
  }
}

case class RModel[+D<:Data](data: D)
sealed trait RInitBase[+D<:Data] {def rval: D; def ren: Option[Bool]}
case class RInit[+D<:Data](rval: D, ren: Option[Bool]) extends RInitBase[D]
object RInit {
  def apply[D<:Data](rval: D): RInit[D] = RInit(rval, None)
  def apply[D<:Data](rval: D, ren: Bool): RInit[D] = RInit(rval, Some(ren))
}
case class RInitModel[+D<:Data](rval: D, ren: Option[Bool]) extends RInitBase[D]
object RInitModel {
  def apply[D<:Data](rval: D): RInitModel[D] = RInitModel(rval, None)
  def apply[D<:Data](rval: D, ren: Bool): RInitModel[D] = RInitModel(rval, Some(ren))
}
