package gama
import internal._

object Wire {
  def apply[D<:Data](model: D)(implicit em: EnclosingModule): D = WireInternals(model, em)
}

