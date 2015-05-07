package gama
package frontend
package implementation

import scala.collection.{immutable=>immutable}

// Activate Scala Language Features
import scala.language.experimental.macros
import macrodefs.{TransformMacro => XFORM}

abstract class VecImpl[D<:Data: Vectorizable](initialModel: D) extends AggregateImpl {
  self: Vec[D] =>
  // D must be invariant because of assignment (:=), amongst other reasons
  // TODO: INDEXED SEQ MAY BE SUBTLY INCORRECT

  // external API
  def :=(source: Data): Unit = macro XFORM.doConnectTo.sourcearg
  def <>(right:  Data): Unit = macro XFORM.doBiConnect.rightarg
  // TODO: CONSIDER: Note how doConnectTo/doBiConnect introduce an implicit
  //   is this OK?

  def lookup(index: Int): D = elements(index)
  def apply(index: Int): D = lookup(index)

  // external->internal API
  def doConnectTo[VFrom<:Data](source: VFrom, info: EnclosureInfo)(implicit writer: ConnectTo[Vec[D],VFrom]): Unit =
    writer.monoConnect(Sink(this), Source(source), info)
  def doBiConnect[RT<:Data](right: RT, info: EnclosureInfo)(implicit writer: BiConnect[Vec[D],RT]): Unit =
    writer.biConnect(Left(this), Right(right), info)
  
  // IMPLEMENTATION BELOW
  protected[gama] val elemType: D = initialModel.copy
  // TODO: BETTER DEFINE WHAT elemType is

  protected[gama] val elements: immutable.IndexedSeq[D] = Vector.fill(length)(elemType.copy)

  def iterator: Iterator[D] = new Iterator[D] { // from IndexedSeqLike.scala
    private[this] var position = 0
    def hasNext: Boolean = position < self.length
    def next(): D = {
      if(position >= self.length) Iterator.empty.next()
      else {
        val d = lookup(position)
        position += 1
        d
      }
    }
  }

  private def enforceElementConsistency(): Unit = {
    elements.foreach(elem => elem.mimic(elemType, asSPEC=false))
      // false b/c exact mimic required: respective nodes of elements should all be completely identical
  }
  protected[gama] def rebind(xform: NodeSpell[_<:Node]): this.type = {
    elemType.rebind(xform)
    enforceElementConsistency()
    this
  }
  protected[gama] def mimic(model: Data, asSPEC: Boolean): Unit = {
    model match {
      case v: Vec[_] => {
        elemType.mimic(v.elemType, asSPEC)
        enforceElementConsistency()
      }
      case _ => throw StructuralMimicException
    }
  }

  def lookupIsConnectable(selector: UIntLike): Boolean = {
    nodes.headOption.getOrElse(elemType) match {
      case _: SPEC => {throw ExpectedNodeException("Synthesizable","SPEC")}
      case _: ConnectableNode    => {NodeCheck.assertConnectable(this); true}
      case _: NonConnectableNode => {NodeCheck.assertNonConnectable(this); false}
    }
  }
  
  def propogateName(newname: NameTree, newsource: NameSource): Unit = {
    elements.zipWithIndex.foreach({case (elem: Data, idx: Int) => 
      elem.forceSetName(NameIndex(this, idx), newsource, true)
    })
  }
  def propogateDescRef(newdesc: Desc): Unit = {
    elements.foreach( elem => {elem.setDescRef(newdesc, true)} )
  }
}

trait VecObjectImpl {
  def apply[D<:Data: Vectorizable](size: Int, model: D): Vec[D] = new Vec(size, model)
//  def apply[D<:Data: Vectorizable](elts: D*)(implicit em: EnclosingModule) = new Vec(elts.toVector)
//  def apply[D<:Data: Vectorizable](elts: immutable.Seq[D])(implicit em: EnclosingModule) = new Vec(elts)
  // mutable.Seq[D] can still be used but must be unpacked into the first construction e.g. Vec(Array(...):_*)
  // Second constructor just prevents unnecessary unpacking and repacking for case of immutable.Seq[D]

  // Basically, Vec can only be regenerated, transfered, etc. if constituent elements are regeneratable or transferable
  implicit def selfMuxer[D<:Data: Muxable]: Muxable[Vec[D]] = new Muxable[Vec[D]] {
    def muxRetVal(tc: Vec[D], fc: Vec[D]) = {
      require(tc.elements.length==fc.elements.length, "Cannot mux together two vectors of different length")
      Vec(tc.elements.length, implicitly[Muxable[D]].muxRetVal(tc.elemType, fc.elemType))
    }
  }
  implicit def connectTo[To<:Data,From<:Data](implicit eltconnect: ConnectTo[To,From]): ConnectTo[Vec[To],Vec[From]] = new ConnectTo[Vec[To],Vec[From]] {
    def monoDetails(sink: Sink[Vec[To]], source: Source[Vec[From]], info: EnclosureInfo): ConnectDetails = {
      require(source.data.elements.length==sink.data.elements.length, "Cannot assign to/from two vectors of different length")
      eltconnect.monoDetails(Sink(sink.data.elemType), Source(source.data.elemType), info) match {
        case ConnectAll => ConnectAll
        case other => ConnectVec(sink.data.elements.length, other)
      }
    }
  }
  implicit def biConnect[LT<:Data,RT<:Data](implicit eltconnect: BiConnect[LT,RT]): BiConnect[Vec[LT],Vec[RT]] = new BiConnect[Vec[LT],Vec[RT]] {
    def biDetails(left: Left[Vec[LT]], right: Right[Vec[RT]], info: EnclosureInfo): BiConnectDetails = {
      require(left.data.elements.length==right.data.elements.length, "Cannot assign to/from two vectors of different length")
      eltconnect.biDetails(Left(left.data.elemType), Right(right.data.elemType), info) match {
        case BiConnectToLeft  => BiConnectToLeft
        case BiConnectToRight => BiConnectToRight
        case other => BiConnectVec(left.data.elements.length, other)
      }
    }
  }
}

