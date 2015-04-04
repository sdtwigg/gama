package gama
import internal._

object Mux {
  //def apply[D<:Data : SelfMuxable](tc: D, fc: D): D = implicitly[SelfMuxable[D]].mux(tc, fc)
  def apply[RT<:Data,TC<:RT,FC<:RT](cond: Bool, tc: TC, fc: FC)(implicit muxer: Muxable[TC, FC], em: EnclosingModule): RT = muxer.mux(cond, tc, fc, em)
  // RT ascription required to ensure return type is appropriately bounded (otherwise Data is assumed)
  //    due to the definition of mux in Muxable
}

