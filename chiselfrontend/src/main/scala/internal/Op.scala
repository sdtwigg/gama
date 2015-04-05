package gama
package internal

trait OpDescImpl {
  self: OpDesc =>
    def validateRetVal(): Unit = NodeCheck.assertOpNode(retVal)
    def genJournalEntry = CreateOp(this)
}

// OpDesc Here:
object UnaryOp {
  def UInt(op: OpIdUnary, input: Element, width: Option[Int], em: EnclosingModule): UInt = {
    NodeCheck.assertSynthesizable(input)
    Desc.generate(new UInt(OpNode(UBits(width),em)))(rv =>
      UnaryOpDesc(op, input, rv, em)
    )
  }
  
  def Bool(op: OpIdUnary, input: Element, em: EnclosingModule): Bool = {
    NodeCheck.assertSynthesizable(input)
    Desc.generate(new Bool(OpNode(UBits(Some(1)),em)))(rv =>
      UnaryOpDesc(op, input, rv, em)
    )
  }
}

object ExtractOp {
  def Bool(input: Element, position: Int, em: EnclosingModule): Bool = {
    NodeCheck.assertSynthesizable(input)
    Desc.generate(new Bool(OpNode(UBits(Some(1)),em)))(rv =>
      ExtractOpDesc(input, position, position, rv, em)
    )
  }

  def UInt(input: Element, left_pos: Int, right_pos: Int, em: EnclosingModule): UInt = {
    NodeCheck.assertSynthesizable(input)
    Desc.generate(new UInt(OpNode(UBits(Some(math.abs(left_pos-right_pos))),em)))(rv =>
      ExtractOpDesc(input, left_pos, right_pos, rv, em)
    )
  }
}

object BinaryOp {
  def UInt(op: OpIdBinary, inputs: Tuple2[Element,Element], em: EnclosingModule): UInt = {
    NodeCheck.assertSynthesizable(inputs._1)
    NodeCheck.assertSynthesizable(inputs._2)
    Desc.generate(new UInt(OpNode(UBits(None),em)))(rv =>
      BinaryOpDesc(op, inputs, rv, em)
    )
  }
  def SInt(op: OpIdBinary, inputs: Tuple2[Element,Element], em: EnclosingModule): SInt = {
    NodeCheck.assertSynthesizable(inputs._1)
    NodeCheck.assertSynthesizable(inputs._2)
    Desc.generate(new SInt(OpNode(SBits(None),em)))(rv =>
      BinaryOpDesc(op, inputs, rv, em)
    )
  }
  def Bool(op: OpIdBinary, inputs: Tuple2[Element,Element], em: EnclosingModule): Bool = {
    NodeCheck.assertSynthesizable(inputs._1)
    NodeCheck.assertSynthesizable(inputs._2)
    Desc.generate(new Bool(OpNode(UBits(Some(1)),em)))(rv => 
      BinaryOpDesc(op, inputs, rv, em)
    )
  }
}


