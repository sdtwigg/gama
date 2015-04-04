package gama
package internal

sealed trait NodeSpell[Out<:Synthesizable] {
  def apply(in: Node, em: EnclosingModule): Out
}

case object RegSpell extends NodeSpell[RegNode] {
  def apply(in: Node, em: EnclosingModule) = new RegNode(in.storage, em)
}
case object WireSpell extends NodeSpell[WireNode] {
  def apply(in: Node, em: EnclosingModule) = new WireNode(in.storage, em)
}
object PortSpell extends NodeSpell[PortNode] {
  def apply(in: Node, em: EnclosingModule) = new PortNode(in.storage, em)
}

case object AccessorSpell extends NodeSpell[AccessorNode] {
  def apply(in: Node, em: EnclosingModule) = new AccessorNode(in.storage, em)
}

object OpCopySpell extends NodeSpell[OpNode] {
  // Don't expect to use this
  def apply(in: Node, em: EnclosingModule) = OpNode(in.storage, em)
}
object OpGenericSpell extends NodeSpell[OpNode] {
  // For Mux, although Mux COULD be clever and try to propogate some width info
  def apply(in: Node, em: EnclosingModule) = OpNode(in.storage.generic, em)
}
