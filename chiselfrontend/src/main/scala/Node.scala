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

class Synthesizable(storage: NodeStore, em: EnclosingModule) extends Node(storage) {
  override def toString = s"${getClass.getSimpleName}@${hashCode.toHexString}_${em.enclosed.hashCode.toHexString}"
}

class Op(storage: NodeStore, em: EnclosingModule) extends Synthesizable(storage, em) {
  em.getActiveJournal.append(CreateOp(this))
}

