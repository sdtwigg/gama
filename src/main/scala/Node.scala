package gama
import internal._

abstract class NodeStore
abstract class RawBits extends NodeStore {
  def width: Option[Int]
}
case class RawUBits(width: Option[Int]) extends RawBits
case class RawSBits(width: Option[Int]) extends RawBits

class Node(val storage: NodeStore)

class SPEC(storage: NodeStore) extends Node(storage) // used only as a placeholder until conversion to 

class Synthesizable(storage: NodeStore, em: EnclosingModule) extends Node(storage) {
  // TODO CONSIDER EnclosingModule versus Module
  em.getOrElse(
    throw new Exception("Synthesizable Nodes must be instantiated inside a (not-None) enclosing module")
  ).getActiveJournal.append(CreateNode(this))
}

class Op(storage: NodeStore, em: EnclosingModule) extends Synthesizable(storage, em)

