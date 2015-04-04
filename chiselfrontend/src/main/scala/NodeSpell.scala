package gama
package internal

case object SynthesizableInvalidException extends ChiselException("Transformation cannot be performed on Synthesizable Nodes.")

sealed trait NodeSpell[Out<:Node] {
  def apply(in: Node): Out
}
sealed trait SPECSpell[Out<:Node] extends NodeSpell[Out] {
  def apply(in: SPEC): Out
  def apply(in: Node) = in match {
    case syn: Synthesizable => {throw SynthesizableInvalidException}
    case spec: SPEC         => (apply(spec))
  }
}

object DirectionSpells {
  import DirectionIO._
  case object SetInput extends SPECSpell[SPEC] {
    def apply(in: SPEC): SPEC = SPEC(in.storage, Some(Input))
  }
  case object SetOutput extends SPECSpell[SPEC] {
    def apply(in: SPEC): SPEC = SPEC(in.storage, Some(Output))
  }
  case object Flip extends SPECSpell[SPEC] {
    private def flip(in: Option[DirectionIO]): Option[DirectionIO] = in match {
      case Some(Input)  => Some(Output)
      case Some(Output) => Some(Input)
      case None => None // TODO: CONSIDER throw error?
    }
    def apply(in: SPEC): SPEC = SPEC(in.storage, flip(in.direction))
  }
}

case class RegSpell(em: EnclosingModule) extends NodeSpell[RegNode] {
  def apply(in: Node) = RegNode(in.storage, em)
}
case class WireSpell(em: EnclosingModule) extends NodeSpell[WireNode] {
  def apply(in: Node) = WireNode(in.storage, em)
}
case class PortSpell(em: EnclosingModule) extends SPECSpell[PortNode] {
  def apply(in: SPEC) = PortNode(in.storage, in.direction.get, em)
}

case class AccessorSpell(em: EnclosingModule) extends NodeSpell[AccessorNode] {
  def apply(in: Node) = AccessorNode(in.storage, em)
}

case class OpCopySpell(em: EnclosingModule) extends NodeSpell[OpNode] {
  // Don't expect to use this
  def apply(in: Node) = OpNode(in.storage, em)
}
case class OpGenericSpell(em: EnclosingModule) extends NodeSpell[OpNode] {
  // For Mux, although Mux COULD be clever and try to propogate some width info
  def apply(in: Node) = OpNode(in.storage.generic, em)
}
