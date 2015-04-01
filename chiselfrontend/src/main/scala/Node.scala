package gama
package internal

abstract class NodeStore
abstract class RawBits extends NodeStore {
  def width: Option[Int]
  override def toString = {
    val simpleWidth: String = width match {
      case Some(width) => width.toString
      case None => "?"
    }
    s"${Console.GREEN}${getClass.getSimpleName}(${simpleWidth})${Console.RESET}"
  }
}
case class UBits(width: Option[Int]) extends RawBits
case class SBits(width: Option[Int]) extends RawBits

class Node(val storage: NodeStore)

class SPEC(storage: NodeStore) extends Node(storage) // used only as a placeholder until conversion to 

trait NodeSpell[Out<:Synthesizable] {
  def apply(in: Node, em: EnclosingModule): Out
}

class Synthesizable(storage: NodeStore, em: EnclosingModule) extends Node(storage) {
  override def toString = s"${Console.YELLOW}${getClass.getSimpleName}@${hashCode.toHexString}${Console.RESET}_${Console.MAGENTA}${em.enclosed.hashCode.toHexString}${Console.RESET}"
}

