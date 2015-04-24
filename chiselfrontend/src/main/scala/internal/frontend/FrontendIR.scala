package gama
package internal
package frontend

import scala.language.existentials

// TODO: WRITE TYPE CHECKER
// TODO: ADD TRANSLATED DEBUGGING INFO

///////////////////
// TREE
sealed trait TreeHW
sealed trait CreatesSymbolHW extends TreeHW { def symbol: SymbolHW }
// Symbol creators
case class WireDecl(symbol: SymbolHW) extends TreeHW with CreatesSymbolHW
case class RegDecl(symbol: SymbolHW) extends TreeHW with CreatesSymbolHW
case class ConstDecl(symbol: SymbolHW, expr: ExprHW) extends TreeHW with CreatesSymbolHW
// Control flow related
case class BlockHW(statements: Seq[TreeHW], upper: BlockHW) extends TreeHW // upper should be weak reference
case class WhenHW(cond: ExprHW, tc: BlockHW, fc: BlockHW) extends TreeHW
// Connection details
case class ConnectStmt(sink: RefHW, source: ExprHW, details: ConnectDetails) extends TreeHW
case class BiConnectStmt(left: RefHW, right: RefHW, details: BiConnectDetails) extends TreeHW
// Memory related
case class MemDecl(desc: MemDesc) extends TreeHW
case class MemRead(symbol: SymbolHW, mem: MemDesc, selector: ExprHW) extends TreeHW with CreatesSymbolHW
case class MemWrite(mem: MemDesc, selector: ExprHW, source: ExprHW) extends TreeHW
  // TODO: Masked and partial mem writes....

///////////////////
// Expressions, References, Types, etc.
sealed trait ExprHW { def resultType: TypeHW } // should these be typed? probably
case class ExprUnary(op: OpIdUnary, target: ExprHW, resultType: TypeHW)
case class ExprBinary(op: OpIdBinary, left: ExprHW, right: ExprHW, resultType: TypeHW)
case class ExprMux(cond: ExprHW, tc: ExprHW, fc: ExprHW, resultType: TypeHW) // resultType IS details
case class ExprLit(litmap: LitMap[_<:Data], resultType: TypeHW) // TODO: convert litmaps?
// Expression version of references
case class ExprRef(ref: RefHW, resultType: TypeHW)
case class ExprVIndex(source: ExprHW, index: Int, resultType: TypeHW)
case class ExprVSelect(source: ExprHW, selector: ExprHW, resultType: TypeHW)
case class ExprTLookup(source: ExprHW, field: String, resultType: TypeHW)
case class ExprExt(source: ExprHW, left_pos: Int, right_pos: Int, resultType: PrimitiveHW)

sealed trait RefHW { def sourceType: TypeHW }
case class SymbolHW(uniqueid: Int, identifier: Option[String], sourceType: TypeHW)
case class VecIndex(parentref: RefHW, index: Int, sourcetype: VecHW)
case class VecSelect(parentref: RefHW, selector: ExprHW, sourcetype: VecHW)
case class TupleLookup(source: RefHW, field: String, sourceType: TupleHW)
case class SubwordExt(source: SymbolHW, left_pos: Int, right_pos: Int, sourceType: PrimitiveHW)

sealed trait TypeHW // TOdO: when converting from journal, memoize type determinations
sealed trait AggregateTypeHW extends TypeHW
case class PrimitiveHW(storage: NodeStore) extends TypeHW
case class TupleHW(fields: Seq[Tuple2[String, TypeHW]]) extends AggregateTypeHW
case class VecHW(depth: Int, elemType: TypeHW) extends AggregateTypeHW

case class MemDesc(memid: Int, identifier: String, sourceType: TypeHW)

//case class VecAccDecl(symbol: SymbolHW, collection: ExprHW, selector: ExprHW) extends TreeHW with CreatesSymbolHW
//case class ExtrDecl(symbol: SymbolHW, left_pos: Int, right_pos: Int) extends TreeHW with CreatesSymbolHW
// Accessor and Extract are converted to expressions or references depending on context:
// if not connectable, then can safely assume an expression
//    also, can safely assume an expression if folded into r-value of a connect or constdecl
// if connectable, then must find the root symbol and build from there
