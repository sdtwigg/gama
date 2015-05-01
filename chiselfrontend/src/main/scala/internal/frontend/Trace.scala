package gama
package internal
package frontend

sealed trait PathTrace
case class PTStart(origin: ExprHW) extends PathTrace
case class PTField(previous: PathTrace, field: String) extends PathTrace
case class PTSelectOne(previous: PathTrace, index: Int) extends PathTrace
case class PTSelectALL(previous: PathTrace) extends PathTrace

sealed trait TypeTrace {def toList: List[TypeTrace]}
case class TTStart(origin: ExprHW) extends TypeTrace {def toList = this :: Nil}
case class TTField(previous: TypeTrace, field: String) extends TypeTrace {def toList = this :: previous.toList}
case class TTIndexALL(previous: TypeTrace) extends TypeTrace {def toList = this :: previous.toList}

object TypeTrace {
  def remake(in: TypeHW, path: Iterable[TypeTrace], reform: TypeHW=>TypeHW): TypeHW = {
    (in, path.headOption) match {
      case (_, None) => reform(in)
      case (_, Some(TTStart(_))) => remake(in, path.tail, reform)
      case (VecHW(d, eType),   Some(TTIndexALL(_))) => VecHW(d, remake(eType, path.tail, reform))
      case (TupleHW(elems), Some(TTField(_, sfield))) => TupleHW(elems.map({case (efield, elem) =>
        (efield, if(sfield == efield) remake(elem, path.tail, reform) else elem)
      }))
      case _ => throw new Exception("Internal Error: Malformed Path")
    }
  }
}
