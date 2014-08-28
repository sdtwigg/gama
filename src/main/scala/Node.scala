package gama

abstract class NodeStore { // These are immutable by design
  // Most of these defs are to allow a generic Mux constructor
  //   (versus every Data type defining a Mux constructor)
  def default: this.type
    // Use of default may be optional but allows alternative to 'guessing' width
    //   and thus on first pass, the tree should only contain user-specified widths
}
case class RawBits(val width: Option[Int]) extends NodeStore {
  def default = RawBits(None).asInstanceOf[this.type]
}
object RawFloat extends NodeStore {
  def default = this
}

// Nodes are highly mutable
abstract class Node[+NS<:NodeStore](initial_storage: NS){
  def inputs: Seq[Node[NodeStore]]

  private[this] var updated_storage: NS = initial_storage
  def storage: NS = updated_storage
  final protected[this] def update_storage(new_storage: NS) = {
    updated_storage = new_storage
  }
}

