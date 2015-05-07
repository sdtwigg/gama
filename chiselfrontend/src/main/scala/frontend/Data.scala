package gama
package frontend

import implementation._
/*
  General contract for copy is that it creates another Data of the same fully derived type (like clone).
  Regarding potential mutable state in Data (and subtypes):
  -> node: all nodes should be connected to a SPEC node,
        with the same NodeStorage as before and direction = node.resolveDirection
  -> name: should not be copied (and thus reset to None)
  -> descRef: should not be copied (and thus reset to None)
*/

sealed trait Data extends Nameable with DescReference {
  def copy: this.type
  protected[gama] def rebind(xform: NodeSpell[_<:Node]): this.type
  protected[gama] def mimic(model: Data, asSPEC: Boolean): Unit
    // asSPEC = true
    //   Used by SimpleCopy in writing the copy function
    //   could easily except if model.type != this.type (widened)
    // asSPEC = false
    //   used by Vec to force elements to all be same node types of elemType
    // TODO: 2 separate functions?

  def nodes: Seq[Node]
  
  import scala.language.experimental.macros
  import implementation.macrodefs.{TransformMacro => XFORM}

  def getWidth: Option[Int]

  def asUInt: UInt = macro XFORM.do_asUInt.noparen
  def connectFromUInt(that: UInt): this.type = macro XFORM.do_connectFromUInt.thatarg
  def fromUInt(that: UInt): this.type = macro XFORM.do_fromUInt.thatarg
  
  def do_asUInt(info: EnclosureInfo): UInt
  def do_connectFromUInt(in: UInt, info: EnclosureInfo): this.type
  def do_fromUInt(in: UInt, info: EnclosureInfo): this.type = {
    val copyWire = Wire.doWire(this, info)
    copyWire.do_connectFromUInt(in, info)
    copyWire.asInstanceOf[this.type]
  }
}

// ELEMENT
abstract class Element(initialNode: Node) extends ElementImpl(initialNode) with Data
  // MUTABLE STATE: node
object Element extends ElementObjectImpl 

// AGGREGATE
sealed trait Aggregate extends AggregateImpl with Data

final class Vec[D<:Data: Vectorizable](val length: Int, initialModel: D) extends VecImpl(initialModel) with Aggregate
  with VecAccessible[D] with Iterable[D]
{  
  def collection = this
  def copy = Vec(length, elemType.copy).asInstanceOf[this.type] // has to be here because of this.type cast
}
object Vec extends VecObjectImpl

abstract class HardwareTuple extends Aggregate with HardwareTupleImpl

@annotation.implicitNotFound("""Cannot create Vec of elements with common type ${D}.
Most common reason is that no self-muxing operation (Muxable[${D}]) available""")
trait Vectorizable[D<:Data] { def muxer: Muxable[D] }
// Only reason Vectorizable trait exists is so that failing to create a Vec gives a specialized error message
object Vectorizable {
  implicit def vectorizer[D<:Data: Muxable]: Vectorizable[D] = new Vectorizable[D] {val muxer = implicitly[Muxable[D]]}
} // TODO: REMOVE DEPENDENCY ON SELFMUXABLE?

