package gama

abstract class Assignable[NS<:NodeStore](storage: NS) extends Node(storage) {
  def forceAssign(source: Node[NS]) 
  def condAssign(source: Node[NS], condition: Node[RawBits])
}

object Wire {
  def apply[T<:Data](model: T): T = {
    val result = model.copy
    result.bind(BindSpell.wire)
    result
  }
}

class Wire[NS<:NodeStore](storage: NS) extends Assignable(storage) {
  private[this] var input: Option[Node[NS]] = None

  def inputs = input.map(Vector(_)).getOrElse(Vector())

  def forceAssign(source: Node[NS]) = {
    input = Option(source)
    println("Work complete")
  }

  def condAssign(source: Node[NS], condition: Node[RawBits]) = ???
}

class Port[NS<:NodeStore](storage: NS, val direction: IODirection) extends Wire(storage) {
}
