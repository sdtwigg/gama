package gama
package intermediate
package passes

object DistributeRef extends GamaPass {
  val name = "DistributeRef"
  // In ExprHW context: this pass moves all RefVIndex, RefVSelect, RefTLookup
  //   as close to the 'roots' as possible (i.e. ExprLit, RefSymbol, RefIO, RefMSelect)
  // DOES NOT MOVE RefExtract (because it could change type)
  // TODO: Move RefExtract?
  // TODO: Simplify ExprLit
  object Transformer extends ExprOnlyTransformTree {
    def attemptSwap(outerref: RefHW, innersource: ExprHW): ExprHW = {
      def rewrap(newsrc: ExprHW): ExprHW = outerref match {
        case RefVIndex(_, index, note)     => attemptSwap(RefVIndex(newsrc, index, note), newsrc)
        case RefVSelect(_, selector, note) => attemptSwap(RefVSelect(newsrc, selector, note), newsrc)
        case RefTLookup(_, field, note)    => attemptSwap(RefTLookup(newsrc, field, note), newsrc)
        case _ => throw new Exception("Catastrophic Error: Non-dependent reference found")
      }
      def simplifyExprLit: ExprHW = (outerref, innersource) match {
        case (RefVIndex(_, index, _), ExprLit(LitVec(elements),_, note)) if index < elements.length =>
          ExprLit(elements(index), outerref.rType, note)
        case (RefTLookup(_, field, _), ExprLit(LitTuple(fields),_, note)) =>
          fields.get(field).map(litv => ExprLit(litv, outerref.rType, note)).getOrElse(outerref)
        case _ => outerref
      }
      innersource match {
        // Swappable Expressions
        case ExprUnary(op, target, _, note) => ExprUnary(op, rewrap(target), outerref.rType, note)
        case ExprBinary(op, left, right, _, note) => ExprBinary(op, rewrap(left), rewrap(right), outerref.rType, note)
        case ExprMux(cond, tc, fc, _, note) => ExprMux(cond, rewrap(tc), rewrap(fc), outerref.rType, note)
        // Cannot swap these expressions so just use old outterref
        case ExprLit(_,_,_) => simplifyExprLit
        case _ => outerref // TODO: Elaborate? Would properly cause compile errors when ExprHW, RefHW, etc. change
      }
    }

    override def transform(expr: ExprHW) = expr match {
      case ref @ RefVIndex(source,_,_)  => {
        val newsrc = transform(source)
        attemptSwap(ref, newsrc)
      }
      case ref @ RefVSelect(source,_,_) => {
        val newsrc = transform(source)
        attemptSwap(ref, newsrc)
      }
      case ref @ RefTLookup(source,_,_) => {
        val newsrc = transform(source)
        attemptSwap(ref, newsrc)
      }

      case _ => super.transform(expr)
    }
  }

  def transform(target: ElaboratedModule): ElaboratedModule = {
    target.copy(body = Transformer.transform(target.body))
  }
}
