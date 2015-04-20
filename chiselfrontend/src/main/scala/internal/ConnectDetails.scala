package gama
package internal

sealed trait ConnectDetails
case object ConnectAll extends ConnectDetails // Used for Element and Aggregates when all fields connect
case class  ConnectTuple(fields: Seq[Tuple2[String,ConnectDetails]]) extends ConnectDetails
case class  ConnectVec(elemdetails: ConnectDetails) extends ConnectDetails

sealed trait BiConnectDetails
case object BiConnectToLeft extends BiConnectDetails // Used for Element and Aggregates when all fields connect
case object BiConnectToRight extends BiConnectDetails // Used for Element and Aggregates when all fields connect
case class  BiConnectTuple(fields: Seq[Tuple2[String,BiConnectDetails]]) extends BiConnectDetails
case class  BiConnectVec(elemdetails: BiConnectDetails) extends BiConnectDetails
