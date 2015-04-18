package gama
import internal._

object Mux {
  def apply[RT<:Data](cond: Bool, tc: RT, fc: RT)(implicit muxer: Muxable[RT], em: EnclosingModule): RT = muxer.mux(cond, tc, fc, em)
  // RT ascription required to ensure return type is appropriately bounded (otherwise Data is assumed)
  //    due to the definition of mux in Muxable
}

