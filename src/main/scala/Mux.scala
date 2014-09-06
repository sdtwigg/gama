package gama

object Mux {
  def apply[TT<:Data,FT<:Data](cond: UInt, tc: TT, fc: FT)(implicit ev: FT <:< TT ): TT = {
    Mux(tc.copy, cond, tc, fc)
  }

  // THIS IS THE MAIN MUX CONSTRUCTOR
  def apply[RT<:Data,TT<:Data,FT<:Data](result: RT, cond: UInt, tc: TT, fc: FT)(
   implicit evi_tc: TT <:< RT, evi_fc: FT <:< RT): RT = {
    // The implicit lines require evidence that TT and RT (the mux inputs) are subtypes of the return.
    //   This is preferable over [TT<:RT,FT<:RT] as now the scala compiler infer RT as the actual
    //   type of result (and not the supertype common to result, tc, and fc, which could be none
    //   of them!).
    result.unsafeMux(cond.getNode, tc, fc)
    result
  }

}

class Mux[NS<:NodeStore](result_storage: NS, cond: Node[RawBits], tc: Node[NS], fc: Node[NS]) extends Op(result_storage) {
  def inputs = Vector(cond, tc, fc)
}
