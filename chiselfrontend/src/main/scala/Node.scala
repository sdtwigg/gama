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

trait NodeSpell[Out<:Synthesizable] {
  def apply(in: Node, em: EnclosingModule): Out
}

case object UnenclosedSynthesizableException extends ChiselException("Synthesizable Node cannot be instantiated without an EnclosingModule")
class Synthesizable(storage: NodeStore, em: EnclosingModule) extends Node(storage) {
  // TODO CONSIDER EnclosingModule versus Module
/*
  def createSelf() = {
    em.getOrElse(
      throw UnenclosedSynthesizableException
    ).getActiveJournal.append(CreateNode(this))
  }
*/
}

class Op(storage: NodeStore, em: EnclosingModule) extends Synthesizable(storage, em) {
  em.getOrElse(
    throw UnenclosedSynthesizableException
  ).getActiveJournal.append(CreateOp(this))
}

