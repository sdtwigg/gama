package gama
package internal
package frontend

// TODO: WRITE TYPE CHECKER
// TODO: ADD TRANSLATED DEBUGGING INFO
sealed trait TreeHW extends Product {
  //override val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  //TODO: Uncommenting this line will ensure hashCode is cached
  //  but also requires all Tree types are truly immutable, which is currently true
  //also, mixin the class name to the hashcode?
}
sealed trait FIRERROR extends TreeHW
///////////////////
// Commands
sealed trait CmdHW extends TreeHW
sealed trait CreatesRefSymbol extends CmdHW { def symbol: RefSymbol }
// Symbol creators
case class WireDecl(symbol: RefSymbol) extends CmdHW with CreatesRefSymbol
case class RegDecl(symbol: RefSymbol) extends CmdHW with CreatesRefSymbol
case class ConstDecl(symbol: RefSymbol, expr: ExprHW) extends CmdHW with CreatesRefSymbol
case class RefDecl(symbol: RefSymbol, ref: RefHW) extends CmdHW with CreatesRefSymbol
  // basically, a reference/pointer... good for holding named accessors/extractors?
  // However, there are other types of 'aliased' connects, like connect on an aggregate
  //   so the presence of this type isn't TOO ridiculous
  // will probably need almost immediate de-aliasing when an l-value side though....
  // de-aliasing pass converts to const (for r-value use) and rewrites l-value use
  // I think can be done after type infer BUT definitley must be de-aliased before connect smashing
// Control flow related
case class BlockHW(statements: List[CmdHW]) extends CmdHW // have reference to enclosing context?
case class WhenHW(cond: ExprHW, tc: BlockHW, fc: BlockHW) extends CmdHW
// Connection details
case class ConnectStmt(sink: RefHW, source: ExprHW, details: ConnectDetails) extends CmdHW
case class BiConnectStmt(left: RefHW, right: RefHW, details: BiConnectDetails) extends CmdHW
// Memory related
case class MemDecl(desc: MemDesc) extends CmdHW
//case class MemRead(symbol: RefSymbol, mem: MemDesc, selector: ExprHW) extends CmdHW with CreatesRefSymbol
//case class MemWrite(mem: MemDesc, selector: ExprHW, source: ExprHW) extends CmdHW
  // TODO: Masked and partial mem writes....
  // TODO: Add these properly
// Other
case class SubModuleDecl(identifier: String, io: RefSymbol) extends CmdHW with CreatesRefSymbol { def symbol = io }
  // TODO: Other fields, like module type

///////////////////
// Expressions, References, Types, etc.
sealed trait ExprHW extends TreeHW { def resultType: TypeHW }
case class ExprUnary(op: OpIdUnary, target: ExprHW, resultType: TypeHW) extends ExprHW
case class ExprBinary(op: OpIdBinary, left: ExprHW, right: ExprHW, resultType: TypeHW) extends ExprHW
case class ExprMux(cond: ExprHW, tc: ExprHW, fc: ExprHW, resultType: TypeHW) extends ExprHW
case class ExprLit(litvalue: LitTree, resultType: TypeHW) extends ExprHW
// References are also all possible expressions // TODO: pass class checks mutability of these
sealed trait RefHW extends ExprHW { def refType: TypeHW; def resultType = refType }
case class RefSymbol(symbol: Int, identifier: Option[String], refType: TypeHW) extends RefHW
case class RefMSelect(mem: MemDesc, selector: ExprHW) extends RefHW {def refType = mem.sourceType}
case class RefVIndex(parent: ExprHW, index: Int, refType: TypeHW) extends RefHW
case class RefVSelect(parent: ExprHW, selector: ExprHW, refType: TypeHW) extends RefHW
case class RefTLookup(source: ExprHW, field: String, refType: TypeHW) extends RefHW
case class RefExtract(source: ExprHW, left_pos: Int, right_pos: Int, refType: TypeHW) extends RefHW
case class RefExprERROR(cause: String) extends RefHW with FIRERROR {def refType = TypeHWUNKNOWN}
// TODO: only RefSymbol actually needs to regen type, others can derive it

sealed trait TypeHW extends TreeHW 
sealed trait PrimitiveTypeHW extends TypeHW {def storage: NodeStore}
case class PrimitiveNode(storage: NodeStore) extends PrimitiveTypeHW
case class PrimitivePort(storage: NodeStore, direction: DirectionIO) extends PrimitiveTypeHW
sealed trait AggregateTypeHW extends TypeHW
case class TupleHW(fields: Vector[Tuple2[String, TypeHW]]) extends AggregateTypeHW
case class VecHW(depth: Int, elemType: TypeHW) extends AggregateTypeHW
case object TypeHWUNKNOWN extends TypeHW with FIRERROR

case class MemDesc(memid: Int, identifier: Option[String], depth: Int, sourceType: TypeHW)

// Literal Details
sealed trait LitTree extends TreeHW
case class LitPrimitive(value: String) extends LitTree
case class LitVec(elements: Vector[LitTree]) extends LitTree
case class LitTuple(fields: Vector[Tuple2[String,LitTree]]) extends LitTree

//case class VecAccDecl(symbol: RefSymbol, collection: ExprHW, selector: ExprHW) extends TreeHW with CreatesRefSymbol
//case class ExtrDecl(symbol: RefSymbol, left_pos: Int, right_pos: Int) extends TreeHW with CreatesRefSymbol
// Accessor and Extract are converted to expressions or references depending on context:
// if not connectable, then can safely assume an expression
//    also, can safely assume an expression if folded into r-value of a connect or constdecl
// if connectable, then must find the root symbol and build from there
// Can have a pass where dig into expressions and greedily try to convert them into references
//   This may not even need to be a separate pass?
// ACTUALLY, notice ConstDecl (which also can't be an l-value), can also grant a RefSymbol
//   Thus, maybe sufficient to just unify the refinements on Expr and Ref?
//      Prefix could be Sel (short for Select)
//      Then ExprRef would actually become ExprSymbol and only take a Symbol
//      Perhaps could inherit from a SelTree and Sel statements all have function saying whether mutable or not
//        function would take a Symbol=>Bool function arg which does the symbol table lookup for RefSymbol
//   Then, verify proper construction via typecheck

// Also, since all of these things are case classes, CSE is VERY straightforward to do
