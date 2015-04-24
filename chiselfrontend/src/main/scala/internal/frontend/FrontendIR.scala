package gama
package internal
package frontend

import scala.language.existentials

// TODO: WRITE TYPE CHECKER
// TODO: ADD TRANSLATED DEBUGGING INFO
sealed trait TreeHW
///////////////////
// Commands
sealed trait CmdHW extends TreeHW
sealed trait CreatesRefSymbol extends CmdHW { def symbol: RefSymbol }
// Symbol creators
case class WireDecl(symbol: RefSymbol) extends CmdHW with CreatesRefSymbol
case class RegDecl(symbol: RefSymbol) extends CmdHW with CreatesRefSymbol
case class ConstDecl(symbol: RefSymbol, expr: ExprHW) extends CmdHW with CreatesRefSymbol
// Control flow related
case class BlockHW(statements: Seq[CmdHW], upper: BlockHW) extends CmdHW // upper should be weak reference
case class WhenHW(cond: ExprHW, tc: BlockHW, fc: BlockHW) extends CmdHW
// Connection details
case class ConnectStmt(sink: RefHW, source: ExprHW, details: ConnectDetails) extends CmdHW
case class BiConnectStmt(left: RefHW, right: RefHW, details: BiConnectDetails) extends CmdHW
// Memory related
case class MemDecl(desc: MemDesc) extends CmdHW
case class MemRead(symbol: RefSymbol, mem: MemDesc, selector: ExprHW) extends CmdHW with CreatesRefSymbol
case class MemWrite(mem: MemDesc, selector: ExprHW, source: ExprHW) extends CmdHW
  // TODO: Masked and partial mem writes....

///////////////////
// Expressions, References, Types, etc.
sealed trait ExprHW extends TreeHW { def resultType: TypeHW }
case class ExprUnary(op: OpIdUnary, target: ExprHW, resultType: TypeHW) extends ExprHW
case class ExprBinary(op: OpIdBinary, left: ExprHW, right: ExprHW, resultType: TypeHW) extends ExprHW
case class ExprMux(cond: ExprHW, tc: ExprHW, fc: ExprHW, resultType: TypeHW) extends ExprHW
case class ExprLit(litmap: LitMap[_<:Data], resultType: TypeHW) extends ExprHW // TODO: convert litmaps? // Leaf
// Expression version of references, may be unncessary?
case class ExprRef(ref: RefHW, resultType: TypeHW) extends ExprHW // Leaf
case class ExprVIndex(source: ExprHW, index: Int, resultType: TypeHW) extends ExprHW
case class ExprVSelect(source: ExprHW, selector: ExprHW, resultType: TypeHW) extends ExprHW
case class ExprTLookup(source: ExprHW, field: String, resultType: TypeHW) extends ExprHW
case class ExprExtract(source: ExprHW, left_pos: Int, right_pos: Int, resultType: PrimitiveHW) extends ExprHW

sealed trait RefHW extends TreeHW { def sourceType: TypeHW }
case class RefSymbol(symbol: Int, identifier: Option[String], sourceType: TypeHW) extends RefHW
case class RefVIndex(parent: RefHW, index: Int, sourceType: VecHW) extends RefHW
case class RefVSelect(parent: RefHW, selector: ExprHW, sourceType: VecHW) extends RefHW
case class RefTLookup(source: RefHW, field: String, sourceType: TupleHW) extends RefHW
case class RefExtract(source: RefHW, left_pos: Int, right_pos: Int, sourceType: PrimitiveHW) extends RefHW

sealed trait TypeHW extends TreeHW // TODO: when converting from journal, memoize type determinations
case class PrimitiveHW(storage: NodeStore) extends TypeHW
sealed trait AggregateTypeHW extends TypeHW
case class TupleHW(fields: Seq[Tuple2[String, TypeHW]]) extends AggregateTypeHW
case class VecHW(depth: Int, elemType: TypeHW) extends AggregateTypeHW

case class MemDesc(memid: Int, identifier: String, sourceType: TypeHW)

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
