package gama

abstract class NodeStore // These are immutable by design
case class RawBits(val width: Option[Int]) extends NodeStore
object Floaty extends NodeStore

// Nodes are highly mutable
abstract class Node[+NS<:NodeStore](initial_storage: NS){
  def inputs: Seq[Node[NodeStore]]

  private[this] var updated_storage: NS = initial_storage
  def storage: NS = updated_storage
  final protected[this] def update_storage(new_storage: NS) = {
    updated_storage = new_storage
  }
}

