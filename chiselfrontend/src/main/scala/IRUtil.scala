package gama

object GamaIRUtil extends GamaIRUtilImpl
trait GamaIRUtilImpl {
  def NOPHW(passid: String): CmdHW = BlockHW(List.empty, GamaNote(GTSourcePass(passid)))
  def NOPHW: CmdHW = BlockHW(List.empty, GamaNote.unknown)
  def ExprLitU(litval: BigInt) = {
    require(litval>=0)
    val width = math.max(litval.bitLength, 1)
    ExprLit(LitRawBits(litval, width, false), PrimitiveNode(UBits(Some(width))), GamaNote.unknown)
  }
  def ExprLitS(litval: BigInt) = {
    val width = litval.bitLength + 1
    ExprLit(LitRawBits(litval, width, false), PrimitiveNode(UBits(Some(width))), GamaNote.unknown)
  }

  def getWidth(in: PrimitiveTypeHW): Option[Option[Int]] = in.storage match {
    case rb: RawBits => (Some(rb.width))
    case _ => None
  }
  def asPrimitiveTypeHW(in: TypeHW): Option[PrimitiveTypeHW] = in match {
    case p @ PrimitiveNode(_)   => Some(p)
    case p @ PrimitivePort(_,_) => Some(p)
    case _ => None
  }
  def calcRefExtractType(in: TypeHW, length: Int): TypeHW = asPrimitiveTypeHW(in).flatMap({
    case PrimitiveNode(_)      => Some( PrimitiveNode(UBits(Some(length))) )
    case PrimitivePort(_, dir) => Some( PrimitivePort(UBits(Some(length)), dir) )
    case _ => None
  }).getOrElse(TypeHWUNKNOWN)

  def asVecHW(in: TypeHW): Option[VecHW] = in match {
    case v @ VecHW(_,_) => Some(v)
    case _ => None
  }
  def getVecEType(in: TypeHW): TypeHW =
    asVecHW(in) map(_.elemType) getOrElse(TypeHWUNKNOWN)
  def getVecDepth(in: TypeHW): Option[Int] = asVecHW(in) map (_.depth)
  
  def asTupleHW(in: TypeHW): Option[TupleHW] = in match {
    case t @ TupleHW(_) => Some(t)
    case _ => None
  }
  def getTupleFType(in: TypeHW, field: String): TypeHW =
    asTupleHW(in) flatMap( _.fields.get(field) ) getOrElse(TypeHWUNKNOWN)
}
