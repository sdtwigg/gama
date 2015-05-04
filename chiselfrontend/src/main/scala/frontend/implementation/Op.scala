package gama
package frontend
package implementation

trait OpDescImpl {
  self: OpDesc =>
    def validateRetVal(): Unit = NodeCheck.assertOpNode(retVal)
    def genJournalEntry = Some(journal.CreateOp(this))
}

// OpDesc Here:
object UnaryOp {
  def UInt(op: OpIdUnary, input: Element, width: Option[Int], info: EnclosureInfo): UInt = {
    NodeCheck.assertSynthesizable(input)
    Desc.generate(new UInt(OpNode(UBits(width),info.em)))(rv =>
      UnaryOpDesc(op, input, rv, info)
    )
  }
  
  def SInt(op: OpIdUnary, input: Element, width: Option[Int], info: EnclosureInfo): SInt = {
    NodeCheck.assertSynthesizable(input)
    Desc.generate(new SInt(OpNode(SBits(width),info.em)))(rv =>
      UnaryOpDesc(op, input, rv, info)
    )
  }
  
  def Bool(op: OpIdUnary, input: Element, info: EnclosureInfo): Bool = {
    NodeCheck.assertSynthesizable(input)
    Desc.generate(new Bool(OpNode(UBits(Some(1)),info.em)))(rv =>
      UnaryOpDesc(op, input, rv, info)
    )
  }
}

object BinaryOp {
  def UInt(op: OpIdBinary, inputs: Tuple2[Element,Element], info: EnclosureInfo): UInt = {
    NodeCheck.assertSynthesizable(inputs._1)
    NodeCheck.assertSynthesizable(inputs._2)
    Desc.generate(new UInt(OpNode(UBits(None),info.em)))(rv =>
      BinaryOpDesc(op, inputs, rv, info)
    )
  }
  def SInt(op: OpIdBinary, inputs: Tuple2[Element,Element], info: EnclosureInfo): SInt = {
    NodeCheck.assertSynthesizable(inputs._1)
    NodeCheck.assertSynthesizable(inputs._2)
    Desc.generate(new SInt(OpNode(SBits(None),info.em)))(rv =>
      BinaryOpDesc(op, inputs, rv, info)
    )
  }
  def Bool(op: OpIdBinary, inputs: Tuple2[Element,Element], info: EnclosureInfo): Bool = {
    NodeCheck.assertSynthesizable(inputs._1)
    NodeCheck.assertSynthesizable(inputs._2)
    Desc.generate(new Bool(OpNode(UBits(Some(1)),info.em)))(rv => 
      BinaryOpDesc(op, inputs, rv, info)
    )
  }
}


