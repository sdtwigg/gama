package gama
package internal

// OpId Here:
sealed abstract class OpId(val shorthand: String)

sealed abstract class OpIdUnary(shorthand: String) extends OpId(shorthand)
case object OpToUInt  extends OpIdUnary("toUInt")
case object OpExtract extends OpIdUnary("extract")

sealed abstract class OpIdBinary(shorthand: String) extends OpId(shorthand)
case object OpPlus   extends OpIdBinary("+")

case object ExpectedSynthesizableException extends ChiselException("Expected node to be of type Synthesizable")
object OpCheck {
  def checkSynthesizable(node: Node): Boolean = node match {
    case syn: Synthesizable => (true)
    case _ => (false)
  }
  def getSynthesizable(node: Node): Synthesizable = node match {
    case syn: Synthesizable => (syn)
    case _ => throw ExpectedSynthesizableException
  }
  def assertSynthesizable(node: Node): Unit = if(!checkSynthesizable(node)) {throw ExpectedSynthesizableException}
  def assertSynthesizable(data: Data): Unit = data.nodes.foreach(node => assertSynthesizable(node))
}

abstract class OpDescImpl {
  self: OpDesc =>
    em.getActiveJournal.append(CreateOp(this))
}

// OpDesc Here:
object UnaryOp {
  def UInt(op: OpIdUnary, input: Element, width: Option[Int], em: EnclosingModule): UInt = {
    OpCheck.assertSynthesizable(input)
    val retVal = new UInt(OpNode(UBits(width),em))
    val newOpDesc = UnaryOpDesc(op, input, retVal, em)
    retVal.descRef = newOpDesc
    retVal
  }
}

object ExtractOp {
  def Bool(input: Element, position: Int, em: EnclosingModule): Bool = {
    OpCheck.assertSynthesizable(input)
    val retVal = new Bool(OpNode(UBits(Some(1)),em))
    val newOpDesc = ExtractOpDesc(input, position, position, retVal, em)
    retVal.descRef = newOpDesc
    retVal
  }

  def UInt(input: Element, left_pos: Int, right_pos: Int, em: EnclosingModule): UInt = {
    OpCheck.assertSynthesizable(input)
    val retVal = new UInt(OpNode(UBits(Some(math.abs(left_pos-right_pos))),em))
    val newOpDesc = ExtractOpDesc(input, left_pos, right_pos, retVal, em)
    retVal.descRef = newOpDesc
    retVal
  }
}

object BinaryOp {
  def UInt(op: OpIdBinary, inputs: Tuple2[Element,Element], em: EnclosingModule): UInt = {
    OpCheck.assertSynthesizable(inputs._1)
    OpCheck.assertSynthesizable(inputs._2)
    val retVal = new UInt(OpNode(UBits(None),em))
    val newOpDesc = BinaryOpDesc(op, inputs, retVal, em)
    retVal.descRef = newOpDesc
    retVal
  }
}


