package gama
package intermediate
package passes

object ExpandVSelectSource extends GamaPass {
  // This pass converts all instances of having a RefVSelect as a source into
  //   a set if muxes with RefVIndex as the sources
  // TODO: Requires RemoveAliases and ExplodeConnects to have been run
  // TODO: Create Mux tree instead of Mux chain
  def transform(target: ElaboratedModule): ElaboratedModule = {
    object Transformer extends ExprOnlyTransformTree {
      override def transform(expr: ExprHW) = expr match {
        case oldrefvs @ RefVSelect(source, selector, note) => {
          val newsrc = transform(source)
          val newsel = transform(selector)
          val depth = getVecDepth(newsrc.rType).getOrElse(0)
          def refvibuild(idx: Int) = RefVIndex(newsrc, idx, oldrefvs.note)
          if(depth > 0) {
            (1 until depth).foldLeft(refvibuild(0): ExprHW)((fc, idx) => {
              val cond = ExprBinary(OpEqual, oldrefvs.selector, ExprLitU(idx), PrimitiveNode(UBits(Some(1))), GamaNote())
              ExprMux(cond, refvibuild(idx), fc, oldrefvs.rType, oldrefvs.note)
            })
          } else RefExprERROR("Bad VecSelect or 0-element Vec when running ExpandVSelectSource")
        }
        case _ => super.transform(expr)
      }
      override def transform(cmd: CmdHW) = cmd match {
        case ConnectStmt(sink, source, details, note) => ConnectStmt(sink, transform(source), details, note)
          // Don't transform sink!!! ExpandVSelectSink does this
        case BiConnectStmt(_,_,_,note) => CmdERROR("BiConnect found during ExpandVSelectSource", note)
        case AliasDecl(_,_,note) => CmdERROR("AliasDecl found during ExpandVSelectSource", note)
        case _ => super.transform(cmd)
      }
    }

    target.copy(body = Transformer.transform(target.body))
  }
}

