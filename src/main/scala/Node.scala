package gama

abstract class NodeStore
case class RawBits(width: Option[Int]) extends NodeStore

class Node[+NS <: NodeStore](val storage: NS)

class Wire[+NS <: NodeStore](storage: NS) extends Node(storage)

