package gama

object GamaIRUtil extends GamaIRUtilImpl
trait GamaIRUtilImpl {
  def getWidth(in: PrimitiveTypeHW): Option[Option[Int]] = in.storage match {
    case rb: RawBits => (Some(rb.width))
    case _ => None
  }
  def asPrimitiveTypeHW(in: TypeHW): Option[PrimitiveTypeHW] = in match {
    case p @ PrimitiveNode(_)   => Some(p)
    case p @ PrimitivePort(_,_) => Some(p)
    case _ => None
  }

  def asVecHW(in: TypeHW): Option[VecHW] = in match {
    case v @ VecHW(_,_) => Some(v)
    case _ => None
  }
  def getVecEType(in: TypeHW): TypeHW =
    asVecHW(in) map(_.elemType) getOrElse(TypeHWUNKNOWN)
  
  def asTupleHW(in: TypeHW): Option[TupleHW] = in match {
    case t @ TupleHW(_) => Some(t)
    case _ => None
  }
  def getTupleFType(in: TypeHW, field: String): TypeHW =
    asTupleHW(in) flatMap( _.fields.get(field) ) getOrElse(TypeHWUNKNOWN)
}
