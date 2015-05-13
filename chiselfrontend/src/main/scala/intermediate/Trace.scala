package gama
package intermediate

sealed trait Trace extends CacheHashCode

// Used by ModuleTypeChecker, SymbolDeclAggregateExplode
sealed trait PathTrace extends Trace
case class PTStart(origin: ExprHW) extends PathTrace
case class PTField(previous: PathTrace, field: String) extends PathTrace
case class PTSelectOne(previous: PathTrace, index: Int) extends PathTrace
case class PTSelectALL(previous: PathTrace) extends PathTrace

// Used by FlattenIO, CollapseConnectsAndScopes
sealed trait IOPathTrace extends Trace
case class IPTStart(modref: ModuleRef) extends IOPathTrace
case class IPTField(previous: IOPathTrace, field: String) extends IOPathTrace
case class IPTSelectOne(previous: IOPathTrace, index: Int) extends IOPathTrace

// Used by the Typers
sealed trait TypeTrace extends Trace {def toList: List[TypeTrace]}
case class TTStart(origin: ExprHW) extends TypeTrace {def toList = this :: Nil}
case class TTField(previous: TypeTrace, field: String) extends TypeTrace {def toList = this :: previous.toList}
case class TTIndexALL(previous: TypeTrace) extends TypeTrace {def toList = this :: previous.toList}

object TypeTrace {
  def remake(in: TypeHW, path: Iterable[TypeTrace], reform: TypeHW=>TypeHW): TypeHW = {
    (in, path.headOption) match {
      case (_, None) => reform(in)
      case (_, Some(TTStart(_))) => remake(in, path.tail, reform)
      case (VecHW(d, eType),   Some(TTIndexALL(_))) => VecHW(d, remake(eType, path.tail, reform))
      case (TupleHW(elems), Some(TTField(_, sfield))) if elems.isDefinedAt(sfield) =>
        TupleHW( elems.updated(sfield, remake(elems(sfield), path.tail, reform)) )
      case _ => throw new Exception("Internal Error: Malformed Path")
    }
  }
}
