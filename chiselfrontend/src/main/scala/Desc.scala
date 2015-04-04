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
      _descRef = Option(in)
      propogateDescRef()
    }
}

sealed trait Desc {val retVal: Data; val em: EnclosingModule}

sealed trait OpDesc extends OpDescImpl with Desc
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
) extends AccessorDescImpl[T] with Desc
