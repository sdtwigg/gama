package gama
import internal._

object Bool {
  def apply() = new Bool(new SPEC(UBits(Some(1))))

  implicit object basicfunctionality extends SelfMuxable[Bool] with SelfTransfer[Bool] {
    def mux(cond: Bool, tc: Bool, fc: Bool)(implicit em: EnclosingModule) = new Bool(new Mux(UBits(Some(1)), cond, tc, fc))
    def selfTransfer(source: Bool, sink: Bool)(implicit em: EnclosingModule) = Element.genSelfTransferImpl(source, sink)(em)
  }

  import scala.language.implicitConversions
  implicit def bool2UInt(in: Bool)(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpToUInt, in.node, 1, em) 
}
class Bool(initialNode: Node) extends Bits(initialNode) {
  def :=(source: Bool)(implicit em: EnclosingModule) = implicitly[SelfTransfer[Bool]].selfTransfer(source, this)
  def copy = new Bool(new SPEC(node.storage)).asInstanceOf[this.type]
}
