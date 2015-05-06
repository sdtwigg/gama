package gama

sealed trait BiConnectDetails {def flipped: BiConnectDetails}
// Used for Element and Aggregates when all fields connect
case object BiConnectToLeft extends BiConnectDetails {def flipped = BiConnectToRight}
case object BiConnectToRight extends BiConnectDetails {def flipped = BiConnectToLeft}
case class  BiConnectTuple(fields: Seq[Tuple2[String,BiConnectDetails]]) extends BiConnectDetails {
  def flipped = BiConnectTuple(fields.map({case (field, elem) => (field, elem.flipped)}))
}
case class  BiConnectVec(numelems: Int, elemdetails: BiConnectDetails) extends BiConnectDetails {
  def flipped = BiConnectVec(numelems, elemdetails.flipped)
}
