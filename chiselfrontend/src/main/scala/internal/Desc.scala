package gama
package internal

// links to whatever OpDesc, Reg, Wire, etc. master instance made me.
trait DescReference { // MUTABLE STATE: descRef
  self: Data =>
    protected[gama] def propogateDescRef(newdesc: Desc): Unit
    
    private[this] var _descRef: Option[scala.ref.WeakReference[Desc]] = None
    protected[gama] def descRef: Option[Desc] = _descRef.flatMap(_.get)
    
    protected[gama] def setDescRef(in: Desc, propogate: Boolean): Unit = {
      assert(_descRef.isEmpty)
      _descRef = Some(scala.ref.WeakReference(in))
      if(propogate) { propogateDescRef(in) }
    }
}

sealed abstract class Desc {
  val retVal: Data

  def doWork(): Unit
  def validateRetVal(): Unit

  doWork()
  validateRetVal()
}
trait EnclosedDesc extends Desc   {
  val info: EnclosureInfo
  def genJournalEntry: Option[journal.Entry]
  // Note: can be enclosed but still not emit a JournalEntry, eg. PortDesc 
  
  def doWork(): Unit = {
    for(entry <- genJournalEntry) yield {
      info.em.getActiveJournal.append(entry)
    }
  }

}
trait UnenclosedDesc extends Desc

sealed abstract class OpDesc extends Desc with OpDescImpl with EnclosedDesc
case class UnaryOpDesc(
  op: OpIdUnary, input: Data,
  retVal: Data, info: EnclosureInfo
) extends OpDesc
case class BinaryOpDesc(
  op: OpIdBinary, inputs: Tuple2[Element,Element],
  retVal: Element, info: EnclosureInfo
) extends OpDesc
case class MuxDesc[T<:Data](
  cond: Bool, tc: T, fc: T,
  retVal: T, info: EnclosureInfo
) extends OpDesc

case class ExtractDesc(
  base: Element, left_pos: Int, right_pos: Int,
  retVal: Element, info: EnclosureInfo
) extends Desc with ExtractDescImpl with EnclosedDesc

case class AccessorDesc[+T<:Data](
  accRef: Accessible[T], selector: UIntLike,
  retVal: T, info: EnclosureInfo
) extends Desc with AccessorDescImpl[T] with EnclosedDesc

case class RegDesc[+T<:Data](retVal: T, info: EnclosureInfo)
 extends Desc with RegDescImpl[T] with EnclosedDesc

case class WireDesc[+T<:Data](retVal: T, info: EnclosureInfo)
 extends Desc with WireDescImpl[T] with EnclosedDesc

case class LitDesc[T<:Data](retVal: T, litMap: LitMap[T])
 extends Desc with LitDescImpl[T] with UnenclosedDesc

object Desc {
  def generate[RV<:Data](retVal: RV)(genDesc: RV=>Desc): RV = {
    val newDesc = genDesc(retVal)
    retVal.setDescRef(newDesc, true)
    retVal
  }
}
