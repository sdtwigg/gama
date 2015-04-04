package gama
package internal

case class ExpectedNodeException(expType: String)
  extends ChiselException(s"Expected node to be of type $expType")

object NodeCheck {
  type DataCheck = Data=>Unit
  def assertNodeBuilder(partialReq: PartialFunction[Node,Boolean], error: =>ChiselException): DataCheck = {
    val lifted = partialReq.lift
    val requirement: Node=>Boolean = (n) => lifted.apply(n).getOrElse(false)
    new Function[Data,Unit] {
      def apply(data: Data): Unit = data match {
        case elem: Element => { if(!requirement(elem.node)) {throw error} }
        case vec: Vec[_]  => { vec.elements.zipWithIndex.foreach({case (elem: Data, idx: Int) =>
          try { apply(elem) }
          catch { case e: ChiselException => {throw TraversalException(s"INDEX $idx", vec.getClass.getName, e)} }
        })}
        case hwt: HardwareTuple => { hwt.subfields.foreach({case (subfield: String, elem: Data) =>
          try { apply(elem) }
          catch { case e: ChiselException => {throw TraversalException(subfield, hwt.getClass.getName, e)} }
        })}
      }
    }
  }
  

  val assertSynthesizable: DataCheck =
    assertNodeBuilder({case syn: Synthesizable => (true)}, ExpectedNodeException("Synthesizable"))
  
  val assertConnectable: DataCheck =
    assertNodeBuilder({case syn: Connectable => (true)}, ExpectedNodeException("Connectable"))
  val assertWireNode: DataCheck =
    assertNodeBuilder({case syn: WireNode => (true)}, ExpectedNodeException("WireNode"))
  val assertRegNode: DataCheck =
    assertNodeBuilder({case syn: RegNode => (true)}, ExpectedNodeException("RegNode"))
  val assertPortNode: DataCheck =
    assertNodeBuilder({case syn: PortNode => (true)}, ExpectedNodeException("PortNode"))
  val assertAccessorNode: DataCheck =
    assertNodeBuilder({case syn: AccessorNode => (true)}, ExpectedNodeException("AccessorNode"))
  
  val assertOpNode: DataCheck =
    assertNodeBuilder({case syn: OpNode => (true)}, ExpectedNodeException("OpNode"))
}
