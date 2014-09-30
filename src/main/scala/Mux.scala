package gama

@annotation.implicitNotFound("Cannot create Mux for ${D}")
trait SelfMuxable[D<:Data] {
  def mux(tc: D, fc: D): D
}

class Mux(storage: NodeStore) extends Op(storage)

object Mux {
  def apply[D<:Data : SelfMuxable](tc: D, fc: D): D = implicitly[SelfMuxable[D]].mux(tc, fc)
}
