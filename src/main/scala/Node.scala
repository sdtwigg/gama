package gama

abstract class NodeStore
abstract class RawBits extends NodeStore {
  def width: Option[Int]
}
case class RawUBits(width: Option[Int]) extends RawBits
case class RawSBits(width: Option[Int]) extends RawBits

class Node[+NS <: NodeStore](val storage: NS)

class Wire[+NS <: NodeStore](storage: NS) extends Node(storage)

class Op[+NS <: NodeStore](storage: NS) extends Node(storage)
