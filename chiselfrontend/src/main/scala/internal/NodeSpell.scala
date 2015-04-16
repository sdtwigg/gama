package gama
package internal

case object SynthesizableTransformException extends ChiselException("Transformations cannot be performed on Synthesizable Nodes.")

sealed trait NodeSpell[+Out<:Node] {
  def apply(in: SPEC): Out
  def apply(in: Node): Out = in match {
    case syn: Synthesizable => {throw SynthesizableTransformException}
    case syn: MemSpec => {throw SynthesizableTransformException} // TODO: CONSIDER: Right type of exception?
    case spec: SPEC => (apply(spec))
  }
}

// These are all meant to be used to internal adjust a node to conform to some property
//   like, become a Reg, adjust direction, etc.
object DirectionSpells {
  import DirectionIO._
  case object SetInput extends NodeSpell[SPEC] {
    def apply(in: SPEC): SPEC = SPEC(in.storage, Some(Input))
  }
  case object SetOutput extends NodeSpell[SPEC] {
    def apply(in: SPEC): SPEC = SPEC(in.storage, Some(Output))
  }
  case object Flip extends NodeSpell[SPEC] {
    private def flip(in: Option[DirectionIO]): Option[DirectionIO] = in match {
      case Some(Input)  => Some(Output)
      case Some(Output) => Some(Input)
      case None => None // TODO: CONSIDER throw error?
    }
    def apply(in: SPEC): SPEC = SPEC(in.storage, flip(in.direction))
  }
}

case class RegSpell(em: EnclosingModule) extends NodeSpell[RegNode] {
  def apply(in: SPEC) = RegNode(in.storage, em)
}
case class WireSpell(em: EnclosingModule) extends NodeSpell[WireNode] {
  def apply(in: SPEC) = WireNode(in.storage, em)
}

case object NoDirectionException extends ChiselException("No Direction Specified")
case class PortSpell(em: EnclosingModule) extends NodeSpell[PortNode] {
  def apply(in: SPEC) = PortNode(in.storage, in.direction.getOrElse(throw NoDirectionException), em)
}

case class ConnectableAccessorSpell(em: EnclosingModule) extends NodeSpell[ConnectableAccessorNode] {
  def apply(in: SPEC) = ConnectableAccessorNode(in.storage, em)
}
case class NonConnectableAccessorSpell(em: EnclosingModule) extends NodeSpell[NonConnectableAccessorNode] {
  def apply(in: SPEC) = NonConnectableAccessorNode(in.storage, em)
}

case class ConnectableExtractedSpell(em: EnclosingModule) extends NodeSpell[ConnectableExtractedNode] {
  def apply(in: SPEC) = ConnectableExtractedNode(in.storage, em)
}
case class NonConnectableExtractedSpell(em: EnclosingModule) extends NodeSpell[NonConnectableExtractedNode] {
  def apply(in: SPEC) = NonConnectableExtractedNode(in.storage, em)
}

case class OpCopySpell(em: EnclosingModule) extends NodeSpell[OpNode] {
  // Don't expect to use this
  def apply(in: SPEC) = OpNode(in.storage, em)
}
case class OpGenericSpell(em: EnclosingModule) extends NodeSpell[OpNode] {
  // For Mux, although Mux COULD be clever and try to propogate some width info
  def apply(in: SPEC) = OpNode(in.storage.generic, em)
}

case class LitAssignSpell(litnode: LitNode) extends NodeSpell[LitNode] {
  // intentionally ignoring SPEC since literals construction from nether
  def apply(in: SPEC) = litnode
}

case class MemSpecSpell(em: EnclosingModule) extends NodeSpell[MemSpec] {
  def apply(in: SPEC) = MemSpec(in.storage, em)
}
