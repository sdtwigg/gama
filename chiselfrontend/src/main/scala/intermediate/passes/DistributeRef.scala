package gama
package intermediate
package passes

object DistributeRef extends GamaPass {
  val name = "DistributeRef"
  // In ExprHW context: this pass moves all RefVIndex, RefVSelect, RefTLookup
  //   as close to the 'roots' as possible (i.e. ExprLit, RefSymbol, RefIO, RefMSelect)
  // 
  object ExprTransformer extends ExprOnlyTransformTree {
    def attemptSwap(outerref: RefHW, innersource: ExprHW): ExprHW = {
      def rewrap(newsrc: ExprHW): ExprHW = outerref match {
        // call attemptSwap again to get this ref as far up the expression tree as possible
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
      // transform source so simplifications done inside-out
      case ref @ RefVIndex(source,_,_)      => attemptSwap(ref, transform(source))
      case ref @ RefVSelect(source,_,_)     => attemptSwap(ref, transform(source))
      case ref @ RefTLookup(source,_,_)     => attemptSwap(ref, transform(source))

      case _ => super.transform(expr)
    }
  }

  object ExtractTransformer extends ExprTransformTreeFullSegregation {
    // RefExtract don't need to 'move-up' reference chains. Just move up Expr chains
    // Also, need to 'eat' other encounter RefExtract
    def attemptExtrAsRef(outerext: RefExtract, innersource: ExprHW): RefHW = {
      innersource match {
        case RefSymbol(_,_,_,_) | RefIO(_,_) | RefMSelect(_,_,_) | RefVIndex(_,_,_) |
             RefVSelect(_,_,_) | RefTLookup(_,_,_) => outerext

        case RefExtract(innerinnersrc, innerLP, innerRP, innerType, _) => {
          // can assume innerinnersrc already transformed and not a RefExtract since inside-out transforms
          // TODO: Handle weird cases where extracting in opposite direction (although this may be an error)
          //   or innerinnersrc is too small (although, perhaps this should infer a fill operation earlier...)
          val innerLength = innerLP - innerRP + 1
          val newLength = math.min(outerext.left_pos - outerext.right_pos + 1, innerLP - innerRP + 1)
          val newRP = innerRP + outerext.right_pos
          val newLP = newRP + newLength - 1
          val newType = outerext.rType match {
            case PrimitivePort(UBits(_), direction) => PrimitivePort(UBits(Some(newLength)), direction)
            case PrimitiveNode(UBits(_)) => PrimitiveNode(UBits(Some(newLength)))
            case _ => TypeHWUNKNOWN // Error case
          } // TODO: Avoidable once other TODO handled, probably, as length only now changes when iisrc too small
          RefExtract(innerinnersrc, newLP, newRP, newType, outerext.note)
        }

        case ExprUnary(_,_,_,_) | ExprBinary(_,_,_,_,_) | ExprMux(_,_,_,_,_) | ExprLit(_,_,_) =>
          RefExprERROR(s"In $name.ExtractTransformer, ExprHW extract source found in RefHW context")
        case RefExprERROR(_) => outerext
      }
    }
    /* // TODO: Actually, ExprLit and ExprMux are simplifiable, but not operations really...
    def attemptExtrAsExpr(outerext: RefExtract, innersource: ExprHW): ExprHW = {
      innersource match {
        case _: RefHW => (attemptExtrAsRef(outerext, innersource))
        case ExprUnary(op, target, innerType, note)
        case ExprBinary(op, 
        case _ => outerext // TODO: full elaborate
      }
    }
    */

    override def transform(ref: RefHW) = ref match {
      case ext @ RefExtract(source,_,_,_,_) => attemptExtrAsRef(ext, transform(source))
      case _ => super.transform(ref)
    }
    /*
    override def transform(expr: ExprHW) = expr match {
      case ext @ RefExtract(source,_,_,_,_) => attemptExtrAsExpr(ext, transform(source))
      case _ => super.transform(expr)
    }
    */
  }

  def transform(target: ElaboratedModule): ElaboratedModule =
    ExtractTransformer.transform(ExprTransformer.transform(target))
}
