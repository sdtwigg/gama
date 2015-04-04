package gama
package internal

@annotation.implicitNotFound("Cannot create Mux for ${D}")
trait SelfMuxable[D<:Data] {
  def muxRetVal(tc: D, fc: D): D
  def mux(cond: Bool, tc: D, fc: D, em: EnclosingModule): D = {
    TraversalException(NodeCheck.assertSynthesizable(cond), "cond", "mux")
    TraversalException(NodeCheck.assertSynthesizable(tc),   "tc", "mux")
    TraversalException(NodeCheck.assertSynthesizable(fc),   "fc", "mux")
    val retVal = muxRetVal(tc, fc).rebind(OpGenericSpell(em))
    val newMux = MuxDesc[D](cond, tc, fc, retVal, em)
    retVal.descRef = newMux
    retVal
  }
}

@annotation.implicitNotFound("Cannot create Mux between ${TC} and ${FC}")
trait Muxable[TC<:Data,FC<:Data] {
  def mux[D<:Data, A >: TC <: D, B >: FC <: D](cond: Bool, tc: TC, fc: FC, em: EnclosingModule): D
}
//also considering Muxable[RT<:Data,TC<:RT,FC<:RT]

object Muxable {
  implicit def selfmuxableconverter[SM<:Data](implicit selfmuxer: SelfMuxable[SM]): Muxable[SM, SM] = new Muxable[SM,SM] {
    def mux[D<:Data, A >: SM <: D, B >: SM <: D](cond: Bool, tc: SM, fc: SM, em: EnclosingModule): D
      = (selfmuxer.mux(cond, tc, fc, em): SM).asInstanceOf[D]
    // Cast required to overcome issue with scala type inferer that won't acknowledge SM <: D
  }
}

