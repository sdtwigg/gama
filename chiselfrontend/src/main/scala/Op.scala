package gama
package internal

sealed abstract class OpId(val shorthand: String)

sealed abstract class OpIdUnary(shorthand: String) extends OpId(shorthand)
case object OpToUInt extends OpIdUnary("toUInt")

sealed abstract class OpIdBinary(shorthand: String) extends OpId(shorthand)
case object OpPlus   extends OpIdBinary("+")

case object ExpectedSynthesizableException extends ChiselException("Expected node to be of type Synthesizable")
abstract class Op(storage: NodeStore, em: EnclosingModule) extends Synthesizable(storage, em) {
  def elaborate: String
  em.getActiveJournal.append(CreateOp(this))
}
object Op {
  def getSynthesizable(node: Node): Synthesizable = node match {
    case syn: Synthesizable => (syn)
    case _ => throw ExpectedSynthesizableException
  }
}

class UnaryOp(op: OpIdUnary, input: Synthesizable, storage: NodeStore, em: EnclosingModule) extends Op(storage, em) {
  def elaborate = s"${op.shorthand}(${input.toString})"
}
object UnaryOp {
  def UInt(op: OpIdUnary, input: Node, em: EnclosingModule): UInt = {
    new UInt(new UnaryOp(op, Op.getSynthesizable(input), UBits(None), em))
  }
  def UInt(op: OpIdUnary, input: Node, width: Int, em: EnclosingModule): UInt = {
    new UInt(new UnaryOp(op, Op.getSynthesizable(input), UBits(Some(width)), em))
  }
}

class BinaryOp(op: OpIdBinary, inputs: Tuple2[Synthesizable,Synthesizable], storage: NodeStore, em: EnclosingModule) extends Op(storage, em) {
  def elaborate = s"(${inputs._1.toString} ${op.shorthand} ${inputs._2.toString})"
}
object BinaryOp {
  def getSynthesizable(inputs: Tuple2[Node,Node]): Tuple2[Synthesizable,Synthesizable] = 
    (Op.getSynthesizable(inputs._1),Op.getSynthesizable(inputs._2))

  def UInt(op: OpIdBinary, inputs: Tuple2[Node,Node], em: EnclosingModule): UInt = {
    new UInt(new BinaryOp(op, getSynthesizable(inputs), UBits(None), em))
  }
}
