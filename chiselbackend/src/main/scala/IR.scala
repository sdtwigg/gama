package gama.IR

sealed trait IODirection
case object InputIO     extends IODirection
case object OutputIO    extends IODirection
case object UnknownDir  extends IODirection

sealed trait IOWidth
case object UnknownWidth          extends IOWidth
case class  IntWidth(value: Int)  extends IOWidth

case class HWOp(op: String)

trait HWType
case object UnknownHWType                   extends HWType
case class  UIntHWType(width: IOWidth)      extends HWType
case class  SIntHWType(width: IOWidth)      extends HWType
case class  BundleType(ports: Seq[HWPort])  extends HWType

case class HWPort(name: String, dir: IODirection, hwtype: HWType)
/*
sealed trait HWNodeRef
case class HWNodeRefTop(name: String)                           extends HWNodeRef
case class HWNodeRefField(top: HWNodeRefTop, subfield: String)  extends HWNodeRef
*/
trait HWCommand
case class  DefLitUInt(ref: String, value: BigInt)                    extends HWCommand
case class  DefLitSInt(ref: String, value: BigInt)                    extends HWCommand
case class  DefWire(ref: String, hwtype: HWType)                      extends HWCommand
case class  DefRegister(ref: String, hwtype: HWType)                  extends HWCommand
case class  DefInstance(ref: String, modulename: String)              extends HWCommand
case class  DefMemory(ref: String, hwtype: HWType, size: Int)         extends HWCommand
case class  DefVector(ref: String, hwtype: HWType, args: Seq[String]) extends HWCommand
case class  DefAccessor(ref: String, src: String, dir: IODirection, idx: String) extends HWCommand
case class  ConnectMany(idx: String, loc: String, exps: Seq[String])  extends HWCommand
case class  ManyConnect(idx: String, locs: Seq[String], exp: String)  extends HWCommand
case class  Begin(body: Seq[HWCommand])                               extends HWCommand
case class  DoHWOp(ref: String, op: HWOp, args: Seq[String])          extends HWCommand
case class  Conditionally(pred: String, tc: HWCommand, fc: HWCommand) extends HWCommand
case object EmptyHWCommand                                            extends HWCommand

