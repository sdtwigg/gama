package gama

case class EnclosingModule(val em: Option[Module[_<:Data]])

abstract class Module[IOT<:Data : Regenerate](makeIO: IOT) {
  implicit val em = EnclosingModule(Some(this))
  final val IOT = Port(makeIO)
}
