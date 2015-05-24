package gama
package frontend

import implementation._

// Activate Scala Language Features
import scala.language.experimental.macros
import implementation.macrodefs.{TransformMacro => XFORM}

object Clock {
  def apply(): Clock = new Clock(new SPEC(ClockNS, None))

  implicit object basicfunctionality
    extends Element.ConnectToImpl[Clock,Clock]
    with Element.BiConnectImpl[Clock,Clock]
}

class Clock(initialNode: Node) extends Analog(initialNode) {
  // TODO: Add to API?

  def copy = new Clock(new SPEC(node.storage, node.resolveDirection)).asInstanceOf[this.type]
  
  // External API
  def :=(source: Clock): Unit = macro XFORM.doConnectTo.sourcearg
  def <>(right:  Clock): Unit = macro XFORM.doBiConnect.rightarg
  
  // external->internal API
  def doConnectTo(source: Clock, info: EnclosureInfo) = 
    ConnectTo[Clock,Clock].monoConnect(Sink(this), Source(source), info)
  def doBiConnect(right: Clock, info: EnclosureInfo): Unit =
    BiConnect[Clock,Clock].biConnect(Left(this), Right(right), info)

  def do_asUInt(info: EnclosureInfo): UInt = ???
  def do_connectFromUInt(in: UInt, info: EnclosureInfo): this.type = ???
  // TODO: Throw proper exceptions here

  // TODO: Better checking for if Clock used (improperly) in a Mux
}
