package gama
package internal

trait OpDescImpl {
  self: OpDesc =>
    em.getActiveJournal.append(CreateOp(this))
}

// OpDesc Here:
object UnaryOp {
  def UInt(op: OpIdUnary, input: Element, width: Option[Int], em: EnclosingModule): UInt = {
    NodeCheck.assertSynthesizable(input)
    val retVal = new UInt(OpNode(UBits(width),em))
    val newOpDesc = UnaryOpDesc(op, input, retVal, em)
    retVal.descRef = newOpDesc
    retVal
  }
}

object ExtractOp {
  def Bool(input: Element, position: Int, em: EnclosingModule): Bool = {
    NodeCheck.assertSynthesizable(input)
    val retVal = new Bool(OpNode(UBits(Some(1)),em))
    val newOpDesc = ExtractOpDesc(input, position, position, retVal, em)
    retVal.descRef = newOpDesc
    retVal
  }

  def UInt(input: Element, left_pos: Int, right_pos: Int, em: EnclosingModule): UInt = {
    NodeCheck.assertSynthesizable(input)
    val retVal = new UInt(OpNode(UBits(Some(math.abs(left_pos-right_pos))),em))
    val newOpDesc = ExtractOpDesc(input, left_pos, right_pos, retVal, em)
    retVal.descRef = newOpDesc
    retVal
  }
}

object BinaryOp {
  def UInt(op: OpIdBinary, inputs: Tuple2[Element,Element], em: EnclosingModule): UInt = {
    NodeCheck.assertSynthesizable(inputs._1)
    NodeCheck.assertSynthesizable(inputs._2)
    val retVal = new UInt(OpNode(UBits(None),em))
    val newOpDesc = BinaryOpDesc(op, inputs, retVal, em)
    retVal.descRef = newOpDesc
    retVal
  }
  def SInt(op: OpIdBinary, inputs: Tuple2[Element,Element], em: EnclosingModule): SInt = {
    NodeCheck.assertSynthesizable(inputs._1)
    NodeCheck.assertSynthesizable(inputs._2)
    val retVal = new SInt(OpNode(SBits(None),em))
    val newOpDesc = BinaryOpDesc(op, inputs, retVal, em)
    retVal.descRef = newOpDesc
    retVal
  }
}


