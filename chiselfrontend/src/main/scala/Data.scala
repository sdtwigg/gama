package gama
import internal._
/*
  General contract for copy is that it creates another Data of the same fully derived type (like clone).
  Regarding potential mutable state in Data (and subtypes):
  -> node: all nodes should be connected to a SPEC node, with the same NodeStorage as before
  -> name: should not be copied (and thus reset to None)
  -> descRef: should not be copied (and thus reset to None)
*/

sealed trait Data extends Nameable with DescReference {
  def copy: this.type
  protected[gama] def rebind(xform: NodeSpell[_<:Node]): this.type

  def nodes: Seq[Node]
}

// ELEMENT
abstract class Element(initialNode: Node) extends ElementImpl(initialNode) with Data
  // MUTABLE STATE: node
object Element extends ElementObjectImpl 

// AGGREGATE
sealed trait Aggregate extends Data

final class Vec[D<:Data: Vectorizable](val length: Int, initialModel: D) extends VecImpl(initialModel) with Aggregate
  with VecAccessible[D] with IndexedSeq[D]
{  
  def collection = this
  def copy = Vec(size, elemType).asInstanceOf[this.type] // has to be here because of this.type cast
}
object Vec extends VecObjectImpl

abstract class HardwareTuple extends Aggregate with HardwareTupleImpl
