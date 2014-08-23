package gama

abstract class NodeStore // These are immutable by design
case class RawBits(val width: Option[Int]) extends NodeStore
object Floaty extends NodeStore


// Nodes are highly mutable
abstract class Node[+NS<:NodeStore](initial_storage: NS){
  private[this] var updated_storage: NS = initial_storage
  def storage: NS = updated_storage
  final protected[gama] def update_storage(new_storage: NodeStore) = {
    updated_storage = new_storage.asInstanceOf[NS]
  }
  // !!!NOTE: Type Safety Workaround: NodeStore so that NS can be covariant
}

class Wire[+NS<:NodeStore](storage: NS) extends Node(storage) {
}

class Port[+NS<:NodeStore](storage: NS, val direction: IODirection) extends Wire(storage) {
}
