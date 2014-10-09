package gama
import internal._

@annotation.implicitNotFound("Cannot create Mux for ${D}")
trait SelfMuxable[D<:Data] {
  def mux(tc: D, fc: D)(implicit em: EnclosingModule): D
}

@annotation.implicitNotFound("Cannot create Mux between ${TC} and ${FC}")
trait Muxable[TC<:Data,FC<:Data] {
  def mux[D<:Data, A >: TC <: D, B >: FC <: D](tc: TC, fc: FC)(implicit em: EnclosingModule): D
}
//also considering Muxable[RT<:Data,TC<:RT,FC<:RT]

object Muxable {
  implicit def selfmuxableconverter[SM<:Data](implicit selfmuxer: SelfMuxable[SM]): Muxable[SM, SM] = new Muxable[SM,SM] {
    def mux[D<:Data, A >: SM <: D, B >: SM <: D](tc: SM, fc: SM)(implicit em: EnclosingModule): D
      = (selfmuxer.mux(tc,fc): SM).asInstanceOf[D]
    // Cast required to overcome issue with scala type inferer that won't acknowledge SM <: D
  }
}

class Mux(storage: NodeStore) extends Op(storage)

object Mux {
  //def apply[D<:Data : SelfMuxable](tc: D, fc: D): D = implicitly[SelfMuxable[D]].mux(tc, fc)
  def apply[RT<:Data,TC<:RT,FC<:RT](cond: Bool, tc: TC, fc: FC)(implicit muxer: Muxable[TC, FC], em: EnclosingModule): RT = muxer.mux(tc, fc)
  // RT ascription required to ensure return type is appropriately bounded (otherwise Data is assumed)
  //    due to the definition of mux in Muxable
}
