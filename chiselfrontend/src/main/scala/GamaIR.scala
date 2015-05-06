package gama

import GamaIRUtil._
// TODO: WRITE TYPE CHECKER
// TODO: ADD TRANSLATED DEBUGGING INFO
sealed trait TreeHW extends Product {
  override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  //TODO: Uncommenting this line will ensure hashCode is cached
  //  but also requires all Tree types are truly immutable, which is currently true
  //also, mixin the class name to the hashcode?
}
// Also, since all of these things are case classes, CSE is VERY straightforward to do
sealed trait FIRERROR extends TreeHW
///////////////////
// Commands
sealed trait CmdHW extends TreeHW { def note: GamaNote }
sealed trait CreatesRefSymbol extends CmdHW { def symbol: RefSymbol }
// Symbol creators
case class WireDecl(symbol: RefSymbol, note: GamaNote) extends CmdHW with CreatesRefSymbol
case class RegDecl(symbol: RefSymbol, reset: Option[Tuple2[ExprHW, ExprHW]], note: GamaNote) extends CmdHW with CreatesRefSymbol
  // Note, first element is boolean for reset enable, second is reset value... Force to be ExprLit?
case class ConstDecl(symbol: RefSymbol, expr: ExprHW, note: GamaNote) extends CmdHW with CreatesRefSymbol
case class AliasDecl(symbol: RefSymbol, ref: RefHW, note: GamaNote) extends CmdHW with CreatesRefSymbol
  // basically, a reference/pointer... good for holding named accessors/extractors?
  // However, there are other types of 'aliased' connects, like connect on an aggregate
  //   so the presence of this type isn't TOO ridiculous
  // will probably need almost immediate de-aliasing when an l-value side though....
  // de-aliasing pass converts to const (for r-value use) and rewrites l-value use
  // I think can be done after type infer BUT definitley must be de-aliased before connect smashing
// Control flow related
case class BlockHW(statements: List[CmdHW], note: GamaNote) extends CmdHW // have reference to enclosing context?
case class WhenHW(cond: ExprHW, tc: CmdHW, fc: CmdHW, note: GamaNote) extends CmdHW
// Connection details
case class ConnectStmt(sink: RefHW, source: ExprHW, details: ConnectDetails, note: GamaNote) extends CmdHW
case class BiConnectStmt(left: RefHW, right: RefHW, details: BiConnectDetails, note: GamaNote) extends CmdHW
// Memory related
case class MemDecl(desc: MemDesc, note: GamaNote) extends CmdHW
//case class MemRead(symbol: RefSymbol, mem: MemDesc, selector: ExprHW) extends CmdHW with CreatesRefSymbol
//case class MemWrite(mem: MemDesc, selector: ExprHW, source: ExprHW) extends CmdHW
  // TODO: Masked and partial mem writes....
  // TODO: Add these properly
// Other
case class SubModuleDecl(details: ModuleSub, ph: String, note: GamaNote) extends CmdHW
  // TODO: Other fields, like module type? ph = placeholder

///////////////////
// Expressions, References, Types, etc.
sealed trait ExprHW extends TreeHW { def rType: TypeHW; def note: GamaNote }
sealed trait ExprLeaf extends ExprHW // for certain lookup tables
case class ExprUnary(op: OpIdUnary, target: ExprHW, rType: TypeHW, note: GamaNote) extends ExprHW
case class ExprBinary(op: OpIdBinary, left: ExprHW, right: ExprHW, rType: TypeHW, note: GamaNote) extends ExprHW
case class ExprMux(cond: ExprHW, tc: ExprHW, fc: ExprHW, rType: TypeHW, note: GamaNote) extends ExprHW
case class ExprLit(litvalue: LitTree, rType: TypeHW, note: GamaNote) extends ExprHW with ExprLeaf
// References are also all possible expressions
sealed trait RefHW extends ExprHW
case class RefSymbol(symbol: Int, identifier: Option[String], rType: TypeHW, note: GamaNote) extends RefHW with ExprLeaf
case class RefIO(mod: ModuleRef, note: GamaNote) extends RefHW with ExprLeaf {def rType = mod.ioType}
case class RefMSelect(mem: MemDesc, selector: ExprHW, note: GamaNote) extends RefHW with ExprLeaf {def rType = mem.sourceType}
case class RefVIndex(source: ExprHW, index: Int, note: GamaNote) extends RefHW {val rType = getVecEType(source.rType)}
case class RefVSelect(source: ExprHW, selector: ExprHW, note: GamaNote) extends RefHW {val rType = getVecEType(source.rType)}
case class RefTLookup(source: ExprHW, field: String, note: GamaNote) extends RefHW {val rType = getTupleFType(source.rType, field)}
case class RefExtract(source: ExprHW, left_pos: Int, right_pos: Int, rType: TypeHW, note: GamaNote) extends RefHW
case class RefExprERROR(cause: String) extends RefHW with FIRERROR {def rType = TypeHWUNKNOWN; def note = GamaNote()}

sealed trait TypeHW extends TreeHW 
sealed trait PrimitiveTypeHW extends TypeHW {def storage: NodeStore}
case class PrimitiveNode(storage: NodeStore) extends PrimitiveTypeHW
case class PrimitivePort(storage: NodeStore, direction: DirectionIO) extends PrimitiveTypeHW
sealed trait AggregateTypeHW extends TypeHW
case class TupleHW(fields: Map[String, TypeHW]) extends AggregateTypeHW
case class VecHW(depth: Int, elemType: TypeHW) extends AggregateTypeHW
case object TypeHWUNKNOWN extends TypeHW with FIRERROR

// Literal Details
sealed trait LitTree extends TreeHW
case class LitRawBits(value: BigInt, width: Int, signed: Boolean) extends LitTree
case class LitVec(elements: Vector[LitTree]) extends LitTree
case class LitTuple(fields: Map[String,LitTree]) extends LitTree

// Miscellaneous data containers
sealed trait ModuleRef extends TreeHW {def ioType: TypeHW}
case class ModuleThis(ioType: TypeHW) extends ModuleRef
case class ModuleSub(modid: Int, identifier: Option[String], ioType: TypeHW) extends ModuleRef

case class MemDesc(memid: Int, identifier: Option[String], depth: Int, sourceType: TypeHW) extends TreeHW

// NOT A TreeHW
case class ElaboratedModule(io: TypeHW, body: CmdHW)

