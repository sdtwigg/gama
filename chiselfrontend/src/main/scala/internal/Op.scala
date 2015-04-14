package gama
package internal

trait OpDescImpl {
  self: OpDesc =>
    def validateRetVal(): Unit = NodeCheck.assertOpNode(retVal)
    def genJournalEntry = Some(CreateOp(this))
}

// OpDesc Here:
object UnaryOp {
  def UInt(op: OpIdUnary, input: Element, width: Option[Int], em: EnclosingModule): UInt = {
    NodeCheck.assertSynthesizable(input)
    Desc.generate(new UInt(OpNode(UBits(width),em)))(rv =>
      UnaryOpDesc(op, input, rv, em)
    )
  }
  
  def SInt(op: OpIdUnary, input: Element, width: Option[Int], em: EnclosingModule): SInt = {
    NodeCheck.assertSynthesizable(input)
    Desc.generate(new SInt(OpNode(SBits(width),em)))(rv =>
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


