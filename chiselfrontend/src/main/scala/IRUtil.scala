package gama

object GamaIRUtil extends GamaIRUtilImpl
trait GamaIRUtilImpl {
  def NOPHW(passid: String): CmdHW = BlockHW(List.empty, GamaNote(GTSourcePass(passid)))
  def NOPHW: CmdHW = BlockHW(List.empty, GamaNote.unknown)

  def ExprLitU(litval: BigInt): ExprLit = ExprLitU(litval, None)
  def ExprLitU(litval: BigInt, width: Int): ExprLit = ExprLitU(litval, Some(width))
  def ExprLitU(litval: BigInt, owidth: Option[Int]): ExprLit = {
    require(litval>=0, "Only non-negative ExprLitU can be constructed")
    val minWidth = math.max(litval.bitLength, 1)
    owidth.foreach(width=>
      require(width >= minWidth, s"Cannot construct ExprLitU of value $litval with width $width (need $minWidth)")
    )
    val width = owidth.getOrElse(minWidth)
    ExprLit(LitRawBits(litval, width, false), PrimitiveNode(UBits(Some(width))), GamaNote.unknown)
  }
  def ExprLitS(litval: BigInt): ExprLit = ExprLitS(litval, None)
  def ExprLitS(litval: BigInt, width: Int): ExprLit = ExprLitS(litval, Some(width))
  def ExprLitS(litval: BigInt, owidth: Option[Int]): ExprLit = {
    val minWidth = litval.bitLength + 1
    owidth.foreach(width=>
      require(width >= minWidth, s"Cannot construct ExprLitS of value $litval with width $width (need $minWidth)")
    )
    val width = owidth.getOrElse(minWidth)
    ExprLit(LitRawBits(litval, width, true), PrimitiveNode(SBits(Some(width))), GamaNote.unknown)
  }

  def ExprLitB(litval: Boolean) = if(litval) ExprLitU(1) else ExprLitU(0)

  def getRawBitsInfo(in: PrimitiveTypeHW): Option[Tuple2[Boolean, Option[Int]]] = getRawBitsInfo(in.storage)
  def getRawBitsInfo(storage: NodeStore): Option[Tuple2[Boolean, Option[Int]]] = storage match {
    case UBits(width) => Some( (false, width) )
    case SBits(width) => Some( (true,  width) )
    case _ => None
  }
  def getWidth(in: PrimitiveTypeHW): Option[Option[Int]] = getWidth(in.storage)
  def getWidth(in: NodeStore): Option[Option[Int]] = getRawBitsInfo(in).map(_._2)
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
    
  def typePort2Node(in: TypeHW): TypeHW = in match {
    case PrimitivePort(storage, _) => PrimitiveNode(storage)
    case VecHW(depth, eType) => VecHW(depth, typePort2Node(eType))
    case TupleHW(fields) => TupleHW(fields.map({ case (field, eType) =>(field, typePort2Node(eType)) }))

    case PrimitiveNode(_) => in
    case TypeHWUNKNOWN => TypeHWUNKNOWN
  }

  def tightenToTarget(source: ExprHW, targetSign: Boolean, targetWidth: Int, addNote: GamaNote): Option[ExprHW] = for{
    (sourceSign, Some(sourceWidth)) <- asPrimitiveTypeHW(source.rType).flatMap(getRawBitsInfo(_))
  } yield {
    if(targetWidth == sourceWidth) {
      if(targetSign == sourceSign) source
      else
        if(targetSign)
          ExprUnary(OpAsSInt, source, PrimitiveNode(SBits(Some(targetWidth))), addNote)
        else
          ExprUnary(OpAsUInt, source, PrimitiveNode(UBits(Some(targetWidth))), addNote)
    }
    else if(targetWidth < sourceWidth) {
      val extract = RefExtract(source, targetWidth-1, 0, addNote)
      if(targetSign)
        ExprUnary(OpAsSInt, extract, PrimitiveNode(SBits(Some(targetWidth))), addNote)
      else extract
    }
    else { // targetWidth > sourceWidth
      val wdiff = targetWidth - sourceWidth
      val allzeros = ExprLitU(0, wdiff)

      val extension = if(sourceSign) {
        val allones = ExprLitU((BigInt(1) << wdiff)-1, wdiff)
        val sourceMSB = RefExtract(source, sourceWidth-1, sourceWidth-1, addNote)
        ExprMux(sourceMSB, allones, allzeros, allzeros.rType, addNote)
      } else allzeros

      val padded = ExprBinary(OpCat, extension, source, PrimitiveNode(UBits(Some(targetWidth))), addNote)
      if(targetSign)
        ExprUnary(OpAsSInt, padded, PrimitiveNode(SBits(Some(targetWidth))), addNote)
      else padded
    }
  }
}
