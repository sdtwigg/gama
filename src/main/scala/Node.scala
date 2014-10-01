package gama

abstract class NodeStore
abstract class RawBits extends NodeStore {
  def width: Option[Int]
}
case class RawUBits(width: Option[Int]) extends RawBits
case class RawSBits(width: Option[Int]) extends RawBits

class Node(val storage: NodeStore)

class SPEC(storage: NodeStore) extends Node(storage) // used only as a placeholder until conversion to 

class Synthesizable(storage: NodeStore) extends Node(storage)

class Op(storage: NodeStore) extends Synthesizable(storage)
