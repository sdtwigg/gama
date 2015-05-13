package gama
package frontend
package implementation
package journal

import scala.language.existentials

sealed trait Entry

case class CreateOp(opdesc: OpDesc) extends Entry
case class CreateWire(wiredesc: WireDesc[_<:Data]) extends Entry
case class CreateReg(regdesc: RegDesc[_<:Data]) extends Entry
case class CreateAccessor(accdesc: AccessorDesc[_<:Data]) extends Entry // into CreateRef
case class CreateExtract(extdesc: ExtractDesc) extends Entry // could be turned into generic CreateRef
case class CreateMem(mem: Mem[_<:Data]) extends Entry
case class CreateModule(module: Module[_<:Data]) extends Entry
case class JMemWrite(mem: Mem[_<:Data], selector: UIntLike, source: Data, mask: Option[UIntLike], info: EnclosureInfo) extends Entry
case class AddBlock(code: Journal) extends Entry
case class Conditionally(cond: Bool, tc: Journal, fc: Journal) extends Entry {NodeCheck.assertSynthesizable(cond)}
case class ConnectData(sink: Sink[Data], source: Source[Data], details: ConnectDetails, info: EnclosureInfo) extends Entry {
  NodeCheck.assertConnectable(sink.data)
  NodeCheck.assertSynthesizable(source.data)
}
case class BiConnectData(left: Left[Data], right: Right[Data], details: BiConnectDetails, info: EnclosureInfo) extends Entry {
  NodeCheck.assertConnectable(left.data)
  NodeCheck.assertConnectable(right.data)
}

