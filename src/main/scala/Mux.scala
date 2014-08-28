package gama

object Mux {
  def apply[T<:Data](cond: UInt, tc: T, fc: T): T = {
    Mux(cond.getNode, tc, fc)
  }

  // This is the actual constructor used by all the others
  private def apply[T<:Data](cond: Node[RawBits], tc: T, fc: T): T = {
    require(tc.getClass == fc.getClass, s"In Mux, tc(${tc.getClass}) and fc(${fc.getClass}) must be same class.")
    require(tc.flatten.length == fc.flatten.length, "In Mux (of ${tc.getClass}), tc and fc structurally different. Likely due to non-determinism or mutability in a subtype of Aggregate.")
    val result_tag = tc.copy
    val opdescs = result_tag.flatten.zip(tc.flatten.zip(fc.flatten))
      // opdescs: (result_tag, (tc, fc))

    opdescs.foreach(opdesc => {
      val op_rtag = opdesc._1; val op_tc = opdesc._2._1; val op_fc = opdesc._2._2
      val opnode = new Mux(op_rtag.generateStorage.default, op_tc.getNode, op_fc.getNode, cond)
      op_rtag.bind(_=>opnode)
    })

    result_tag
  }
}

class Mux[NS<:NodeStore](result_storage: NS, tc: Node[NS], fc: Node[NS], cond: Node[RawBits]) extends Op(result_storage) {
  def inputs = Vector(tc, fc, cond)
}
