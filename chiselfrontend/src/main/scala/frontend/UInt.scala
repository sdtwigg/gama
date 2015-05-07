package gama
package frontend

import implementation._

// Activate Scala Language Features
import scala.language.experimental.macros
import implementation.macrodefs.{TransformMacro => XFORM}

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
  // OKAY-ness discussed in BiConnect.scala
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
    ConnectTo[UInt,Digital].monoConnect(Sink(this), Source(source), info)
  def doBiConnect(right: UInt, info: EnclosureInfo): Unit =
    BiConnect[UInt,UInt].biConnect(Left(this), Right(right), info)
  def doBiConnect(right: SInt, info: EnclosureInfo): Unit =
    BiConnect[UInt,SInt].biConnect(Left(this), Right(right), info)
  // TODO: See above
  
  // implementation for parent abstracts
  def do_connectFromUInt(in: UInt, info: EnclosureInfo): this.type = {
    this.doConnectTo(in, info)
    this
  }

  def do_not(info: EnclosureInfo): Self = UnaryOp.UInt(OpNot, this, this.getWidth, info)
  
  // Internal API
  protected[gama] def getLiteralValue: Option[Int] = descRef.flatMap({
    case LitDesc(_, lmap) => Some(lmap.asLitTree)
    case _ => None
  }).flatMap({
    case LitRawBits(value, _, _) => Some(value.toInt)
    case _ => None
  })
}
