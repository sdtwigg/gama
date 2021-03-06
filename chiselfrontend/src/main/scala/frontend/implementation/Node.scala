package gama
package frontend
package implementation

sealed abstract class Node {
  val storage: NodeStore
  def oem: Option[EnclosingModule]
  def resolveDirection: Option[DirectionIO]
}
sealed trait EnclosedNode {val em: EnclosingModule; def oem = Some(em)}
sealed trait UnenclosedNode {def oem = None}

// used only as a placeholder until conversion to 
case class SPEC(storage: NodeStore, direction: Option[DirectionIO]) extends Node with UnenclosedNode {
  def resolveDirection = direction
}

case class MemSpec(storage: NodeStore, em: EnclosingModule) extends Node with UnenclosedNode {
  def resolveDirection = None
}
  // TODO: CONSIDER: Not clear if this is even necessary

sealed trait NoDirectionNode {def resolveDirection = None}
sealed abstract class Synthesizable extends Node

sealed abstract class ConnectableNode extends Synthesizable with EnclosedNode
case class WireNode(storage: NodeStore, em: EnclosingModule) extends ConnectableNode with NoDirectionNode
case class RegNode(storage: NodeStore, em: EnclosingModule) extends ConnectableNode with NoDirectionNode
case class PortNode(storage: NodeStore, direction: DirectionIO, em: EnclosingModule) extends ConnectableNode {def resolveDirection = Some(direction)}

// TODO: NonConnectableNode may be unnecessary
sealed abstract class NonConnectableNode extends Synthesizable
case class OpNode(storage: NodeStore, em: EnclosingModule)  extends NonConnectableNode with EnclosedNode with NoDirectionNode
case class LitNode(storage: NodeStore) extends NonConnectableNode with UnenclosedNode with NoDirectionNode //formerly had LitNodeDesc

sealed trait AccessorNode extends Synthesizable {def optDirection: Option[DirectionIO]; def resolveDirection = optDirection}
case class ConnectableAccessorNode(storage: NodeStore, optDirection: Option[DirectionIO], em: EnclosingModule) extends ConnectableNode with AccessorNode
case class NonConnectableAccessorNode(storage: NodeStore, optDirection: Option[DirectionIO], em: EnclosingModule) extends NonConnectableNode with AccessorNode with EnclosedNode
// Theoretically, could have a UnenclosedAccessorNode; however, unlikely and so will ignore for now
//   as for similar reasons could have UnenclosedOpNodes

sealed trait ExtractedNode extends Synthesizable {def optDirection: Option[DirectionIO]; def resolveDirection = optDirection}
case class ConnectableExtractedNode(storage: NodeStore, optDirection: Option[DirectionIO], em: EnclosingModule) extends ConnectableNode with ExtractedNode
case class NonConnectableExtractedNode(storage: NodeStore, optDirection: Option[DirectionIO], em: EnclosingModule) extends NonConnectableNode with ExtractedNode with EnclosedNode
// TODO: Is there really a difference between extracting and accessing??
