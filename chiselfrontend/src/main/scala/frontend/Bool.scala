package gama
package frontend

import implementation._
  
// Activate Scala Language Features
import scala.language.experimental.macros
import implementation.macrodefs.{TransformMacro => XFORM}

object Bool {
  def apply() = new Bool(SPEC(UBits(Some(1)), None))
  
  implicit object basicfunctionality
    extends Muxable[Bool]
    with Element.ConnectToImpl[Bool,Bool]
    with Element.BiConnectImpl[Bool,Bool]
  {
    def muxRetVal(tc: Bool, fc: Bool): Bool = Bool()
  }
  //import scala.language.implicitConversions
  //implicit def bool2UInt(in: Bool)(implicit em: EnclosingModule): UInt = UnaryOp.UInt(OpToUInt, in, Some(1), em) 
}
final class Bool(initialNode: Node) extends UIntLike(initialNode) {
  protected type Self = Bool
  
  def copy = new Bool(SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]

  // External API
  def :=(source: Bool): Unit = macro XFORM.doConnectTo.sourcearg
  def <>(right:  Bool): Unit = macro XFORM.doBiConnect.rightarg
  
  def unary_!(): Bool = macro XFORM.do_not.paren

  def &&(that: Bool): Bool = macro XFORM.do_andB.thatarg
  def ||(that: Bool): Bool = macro XFORM.do_orB.thatarg
  def ^^(that: Bool): Bool = macro XFORM.do_xorB.thatarg
  
  // external->internal API
  def doConnectTo(source: Bool, info: EnclosureInfo) = 
    ConnectTo[Bool,Bool].monoConnect(Sink(this), Source(source), info)
  def doBiConnect(right: Bool, info: EnclosureInfo): Unit =
    BiConnect[Bool,Bool].biConnect(Left(this), Right(right), info)
  
  def do_connectFromUInt(in: UInt, info: EnclosureInfo): this.type = {
    this.doConnectTo(in.doExtract(0, info), info)
    this
  }
  
  def do_not(info: EnclosureInfo): Self = UnaryOp.Bool(OpNot, this, info)
  
  def do_andB(that: Bool, info: EnclosureInfo): Bool = BinaryOp.Bool(OpAnd, (this, that), info)
  def do_orB (that: Bool, info: EnclosureInfo): Bool = BinaryOp.Bool(OpOr,  (this, that), info)
  def do_xorB(that: Bool, info: EnclosureInfo): Bool = BinaryOp.Bool(OpXor, (this, that), info)
}
