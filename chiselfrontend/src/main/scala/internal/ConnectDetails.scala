package gama
package internal

sealed trait ConnectDetails
case object ConnectAll extends ConnectDetails // Used for Element and Aggregates when all fields connect
case class  ConnectTuple(fields: Seq[Tuple2[String,ConnectDetails]]) extends ConnectDetails
case class  ConnectVec(elemdetails: ConnectDetails) extends ConnectDetails
