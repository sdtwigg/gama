package gama
package internal

// links to whatever OpDesc, Reg, Wire, etc. master instance made me.
trait DescReference { // MUTABLE STATE: descRef
  self: Data =>
    private[this] var _descRef: Option[Desc] = None
    protected[gama] def descRef: Option[Desc] = _descRef
    protected[gama] def propogateDescRef(): Unit
    protected[gama] def descRef_=(in: Desc): Unit = {
      assert(_descRef.isEmpty)
      _descRef = Some(in)
      propogateDescRef()
    }
}

sealed abstract class Desc {
  val retVal: Data
  val em: EnclosingModule

  def validateRetVal(): Unit
  def genJournalEntry: JournalEntry

  validateRetVal()
  em.getActiveJournal.append(genJournalEntry)
}

sealed abstract class OpDesc extends Desc with OpDescImpl
case class UnaryOpDesc(
  op: OpIdUnary, input: Element,
  retVal: Element, em: EnclosingModule
) extends OpDesc
case class ExtractOpDesc(
  input: Element, left_pos: Int, right_pos: Int,
  retVal: Element, em: EnclosingModule
) extends OpDesc
case class BinaryOpDesc(
  op: OpIdBinary, inputs: Tuple2[Element,Element],
  retVal: Element, em: EnclosingModule
) extends OpDesc
case class MuxDesc[T<:Data](
  cond: Bool, tc: T, fc: T,
  retVal: T, em: EnclosingModule
) extends OpDesc

case class AccessorDesc[+T<:Data](
  collection: Accessible[T], selector: UInt,
  retVal: T, em: EnclosingModule
) extends Desc with AccessorDescImpl[T]

case class RegDesc[+T<:Data](retVal: T, em: EnclosingModule)
 extends Desc with RegDescImpl[T]
case class WireDesc[+T<:Data](retVal: T, em: EnclosingModule)
 extends Desc with WireDescImpl[T]

object Desc {
  def generate[RV<:Data](retVal: RV)(genDesc: RV=>Desc): RV = {
    val newDesc = genDesc(retVal)
    retVal.descRef = newDesc
    retVal
  }
}
