package gama
package internal
package frontend

sealed trait PathTrace
case class PTStart(origin: ExprHW) extends PathTrace
case class PTField(previous: PathTrace, field: String) extends PathTrace
case class PTSelectOne(previous: PathTrace, index: Int) extends PathTrace
case class PTSelectALL(previous: PathTrace) extends PathTrace

sealed trait TypeTrace
case class TTStart(origin: ExprHW) extends TypeTrace
case class TTField(previous: TypeTrace, field: String) extends TypeTrace
case class TTIndexALL(previous: TypeTrace) extends TypeTrace
