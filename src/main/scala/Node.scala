package gama

abstract class NodeStore // These are immutable by design
case class RawBits(val width: Option[Int]) extends NodeStore
object Floaty extends NodeStore

// Nodes are highly mutable
abstract class Node[+RD<:NodeStore](initial_storage: RD){
  private var updated_storage: NodeStore = initial_storage
  def storage: RD = updated_storage.asInstanceOf[RD]
  def update_storage(new_storage: NodeStore) = {
    if(new_storage.getClass == initial_storage.getClass) {
      updated_storage = new_storage
    } else {throw new Exception("Attempted to replace Node's storage with incompatible type") }
  }
  // !!!NOTE: Type Safety Workaround: NodeStore so that RD can be covariant
}

class Wire[+RD<:NodeStore](storage: RD) extends Node(storage) {
}

class Port[+RD<:NodeStore](storage: RD, direction: IODirection) extends Wire(storage) {
}
