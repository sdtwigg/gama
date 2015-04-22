package gama
import internal._

// Activate Scala Language Features
import scala.language.experimental.macros
import gama.internal.macrodefs.{TransformMacro => XFORM}

object UInt {
  def apply(): UInt           = apply(None)
  def apply(width: Int): UInt = apply(Some(width))
  def apply(width: Option[Int]) = new UInt(new SPEC(UBits(width), None))
  
  implicit object basicfunctionality
    extends Muxable[UInt]
    with Element.ConnectToImpl[UInt,Digital]
    with Element.BiConnectImpl[UInt,UInt]
  {
    def muxRetVal(tc: UInt, fc: UInt): UInt = UInt()
  }
  implicit object withsintfunctionality
    extends Element.BiConnectImpl[UInt,SInt]
  // TODO: Is this actually ok? Not always clear what is going on....
}
// Placing object first lets the class find the implicits in the object
final class UInt(initialNode: Node) extends UIntLike(initialNode) {
  protected type Self = UInt
  
  def copy = new UInt(new SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
 
  // External API
  def :=(source: Digital): Unit = macro XFORM.doConnectTo.sourcearg
  def <>(right:  UInt): Unit = macro XFORM.doBiConnect.rightarg
  def <>(right:  SInt): Unit = macro XFORM.doBiConnect.rightarg
  // TODO: there are ways to reduce duplication here
  //    main disadvantage is cleaning up error messages when user does wrong thing

  // external->internal API
  def doConnectTo(source: Digital, info: EnclosureInfo) = 
    ConnectTo[UInt,Digital].monoConnect(Sink(this), Source(source), info.em)
  def doBiConnect(right: UInt, info: EnclosureInfo): Unit =
    BiConnect[UInt,UInt].biConnect(Left(this), Right(right), info.em)
  def doBiConnect(right: SInt, info: EnclosureInfo): Unit =
    BiConnect[UInt,SInt].biConnect(Left(this), Right(right), info.em)
  // TODO: See above

  
  // implementation for parent abstracts
  def do_not(info: EnclosureInfo): Self = UnaryOp.UInt(OpNot, this, this.getWidth, info)
}
