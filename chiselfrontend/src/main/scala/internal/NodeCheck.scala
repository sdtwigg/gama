package gama
package internal

case class ExpectedNodeException(expType: String, actualType: String)
  extends ChiselException(s"Expected node to be of type $expType, is $actualType")
object ExpectedNodeException {
  def partial(expType: String): String => ExpectedNodeException =
    (actualType: String) => ExpectedNodeException(expType, actualType)
}

object NodeCheck {
  type DataCheck = Data=>Unit
  def assertNodeBuilder(partialReq: PartialFunction[Node,Boolean], error: String=>ExpectedNodeException): DataCheck = {
    val lifted = partialReq.lift
    val requirement: Node=>Boolean = (n) => lifted.apply(n).getOrElse(false)
    new Function[Data,Unit] {
      def apply(data: Data): Unit = data match {
        case elem: Element => { if(!requirement(elem.node)) {throw error(elem.node.getClass.getName)} }
        case vec: Vec[_]  => { vec.zipWithIndex.foreach({case (elem: Data, idx: Int) =>
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
    assertNodeBuilder({case syn: Synthesizable => (true)}, ExpectedNodeException.partial("Synthesizable"))
  
  val assertConnectable: DataCheck =
    assertNodeBuilder({case syn: Connectable => (true)}, ExpectedNodeException.partial("Connectable"))
  val assertWireNode: DataCheck =
    assertNodeBuilder({case syn: WireNode => (true)}, ExpectedNodeException.partial("WireNode"))
  val assertRegNode: DataCheck =
    assertNodeBuilder({case syn: RegNode => (true)}, ExpectedNodeException.partial("RegNode"))
  val assertPortNode: DataCheck =
    assertNodeBuilder({case syn: PortNode => (true)}, ExpectedNodeException.partial("PortNode"))
  val assertAccessorNode: DataCheck =
    assertNodeBuilder({case syn: AccessorNode => (true)}, ExpectedNodeException.partial("AccessorNode"))
  
  val assertNonConnectable: DataCheck =
    assertNodeBuilder({case syn: NonConnectable => (true)}, ExpectedNodeException.partial("NonConnectable"))
  val assertOpNode: DataCheck =
    assertNodeBuilder({case syn: OpNode => (true)}, ExpectedNodeException.partial("OpNode"))
}
