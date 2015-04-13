package gama
import internal._

object AsHWConstant {
  def apply[D<:Data](in: D)(implicit em: EnclosingModule): D = {
    NodeCheck.assertSynthesizable(in)
    Desc.generate(in.copy.rebind(OpCopySpell(em)))(rv =>
      UnaryOpDesc(OpIDENT, in, rv, em)
    )
  }
}
