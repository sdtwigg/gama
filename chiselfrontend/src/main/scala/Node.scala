package gama
package internal

abstract class NodeStore {def generic: NodeStore} // generic used by OpGenericSpell
abstract class RawBits extends NodeStore {def width: Option[Int]}
case class UBits(width: Option[Int]) extends RawBits {def generic = UBits(None)}
case class SBits(width: Option[Int]) extends RawBits {def generic = SBits(None)}

sealed trait Node {val storage: NodeStore; def resolveDirection: Option[DirectionIO]}

// used only as a placeholder until conversion to 
case class SPEC(storage: NodeStore, direction: Option[DirectionIO]) extends Node {
  def resolveDirection = direction
}

sealed trait Synthesizable extends Node {val em: EnclosingModule; def resolveDirection: Option[DirectionIO] = None}

sealed trait Connectable extends Synthesizable 
case class WireNode(storage: NodeStore, em: EnclosingModule) extends Connectable
case class RegNode(storage: NodeStore, em: EnclosingModule) extends Connectable
case class PortNode(storage: NodeStore, direction: DirectionIO, em: EnclosingModule) extends Connectable {override def resolveDirection = Some(direction)}
case class AccessorNode(storage: NodeStore, em: EnclosingModule) extends Connectable // TODO: Extending Connectable isn't quite right....

sealed trait NonConnectable extends Synthesizable
case class OpNode(storage: NodeStore, em: EnclosingModule) extends NonConnectable
