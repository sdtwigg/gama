package gama

object BinaryOp {
  def apply[RVT<:Element[NodeStore],PT<:Element[NodeStore]](return_tag: RVT, operand1: PT, operand2: PT, opcode: BinaryOpcode) = {
    val opnode = new BinaryOp(return_tag.generateStorage, operand1.getNode, operand2.getNode, opcode)
      // create Op Node  (note: using the return tag's storage for the Op Node)
    return_tag.bind(_=>opnode) // bind tag to the Op Node
    return_tag
  }
}

abstract class Op[NS<:NodeStore](result_storage: NS) extends Node(result_storage)

//class UnaryOp[NS<:NodeStore]


class BinaryOp[NSR<:NodeStore,NSP<:NodeStore](result_storage: NSR, op1: Node[NSP], op2: Node[NSP], opcode: BinaryOpcode) extends Op(result_storage) {
  def inputs = Vector(op1, op2)
}

