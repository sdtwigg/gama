package gama

//case class EnclosingModule(val em: Module[_<:Data])

abstract class Module[IOT<:Data : Regenerate](makeIO: IOT) {
  final val IOT = Port(makeIO)
}
