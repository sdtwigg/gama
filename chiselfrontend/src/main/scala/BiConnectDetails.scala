package gama

sealed trait BiConnectDetails
case object BiConnectToLeft extends BiConnectDetails // Used for Element and Aggregates when all fields connect
case object BiConnectToRight extends BiConnectDetails // Used for Element and Aggregates when all fields connect
case class  BiConnectTuple(fields: Seq[Tuple2[String,BiConnectDetails]]) extends BiConnectDetails
case class  BiConnectVec(elemdetails: BiConnectDetails) extends BiConnectDetails
