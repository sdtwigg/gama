package gama
import internal._

object AsHWConstant {
  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}

  // External API
  def apply[D<:Data](in: D): D = macro XFORM.doConstant.xform[D]

  // external->internal API
  def doConstant[D<:Data](in: D, info: EnclosureInfo): D = {
    NodeCheck.assertSynthesizable(in)
    Desc.generate(in.copy.rebind(OpSpell(info.em)))(rv =>
      UnaryOpDesc(OpIDENT, in, rv, info)
    )
  }
}
