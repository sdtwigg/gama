package gama
package internal
package frontend

sealed trait PathTrace
case class PTStart(origin: ExprHW) extends PathTrace
case class PTField(previous: PathTrace, field: String) extends PathTrace
case class PTSelectOne(previous: PathTrace, index: Int) extends PathTrace
case class PTSelectALL(previous: PathTrace) extends PathTrace
