package gama
package internal

abstract class NodeStore {def generic: NodeStore} // generic used by OpGenericSpell
sealed abstract class RawBits extends NodeStore {def width: Option[Int]}
case class UBits(width: Option[Int]) extends RawBits {def generic = UBits(None)}
case class SBits(width: Option[Int]) extends RawBits {def generic = SBits(None)}

sealed abstract class Node {val storage: NodeStore; def resolveDirection: Option[DirectionIO]}

// used only as a placeholder until conversion to 
case class SPEC(storage: NodeStore, direction: Option[DirectionIO]) extends Node {
  def resolveDirection = direction
}

case class MemSpec(storage: NodeStore, em: EnclosingModule) extends Node {def resolveDirection = None}
  // TODO: CONSIDER: Not clear if this is even necessary

sealed abstract class Synthesizable extends Node {def oem: Option[EnclosingModule]; def resolveDirection: Option[DirectionIO] = None}
trait EnclosedNode extends Synthesizable   {val em: EnclosingModule; def oem = Some(em)}
trait UnenclosedNode extends Synthesizable {def oem = None}

sealed abstract class Connectable extends Synthesizable with EnclosedNode
case class WireNode(storage: NodeStore, em: EnclosingModule) extends Connectable
case class RegNode(storage: NodeStore, em: EnclosingModule) extends Connectable
case class PortNode(storage: NodeStore, direction: DirectionIO, em: EnclosingModule) extends Connectable {override def resolveDirection = Some(direction)}

// TODO: NonConnectable may be unnecessary
sealed abstract class NonConnectable extends Synthesizable
case class OpNode(storage: NodeStore, em: EnclosingModule)  extends NonConnectable with EnclosedNode
case class LitNode(storage: NodeStore) extends NonConnectable with UnenclosedNode //formerly had LitNodeDesc

sealed trait AccessorNode extends Synthesizable
case class ConnectableAccessorNode(storage: NodeStore, em: EnclosingModule) extends Connectable with AccessorNode
case class NonConnectableAccessorNode(storage: NodeStore, em: EnclosingModule) extends NonConnectable with AccessorNode with EnclosedNode
// Theoretically, could have a UnenclosedAccessorNode; however, unlikely and so will ignore for now
//   as for similar reasons could have UnenclosedOpNodes
