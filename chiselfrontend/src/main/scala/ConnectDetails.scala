package gama

sealed trait ConnectDetails
case object ConnectAll extends ConnectDetails // Used for Element and Aggregates when all fields connect
case class  ConnectTuple(fields: Seq[Tuple2[String,ConnectDetails]]) extends ConnectDetails
case class  ConnectVec(numelems: Int, elemdetails: ConnectDetails) extends ConnectDetails
