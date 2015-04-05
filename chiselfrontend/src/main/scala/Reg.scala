package gama
import internal._

object Reg {
  def apply[D<:Data](model: D)(implicit em: EnclosingModule): D = RegInternals(model, em)
}

