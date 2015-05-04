package gama
package frontend
package implementation

object MuxableImpl {
  def mux[D<:Data](origin: Muxable[D], cond: Bool, tc: D, fc: D, info: EnclosureInfo): D = {
    TraversalException(NodeCheck.assertSynthesizable(cond), "cond", "mux")
    TraversalException(NodeCheck.assertSynthesizable(tc),   "tc", "mux")
    TraversalException(NodeCheck.assertSynthesizable(fc),   "fc", "mux")
    Desc.generate(origin.muxRetVal(tc, fc).rebind(GenericizeSpell).rebind(OpSpell(info.em)))(rv =>
      MuxDesc(cond, tc, fc, rv, info)
    )
      // TODO: GenericizeSpell MAY be unnecessary if care is taken
  }
}

