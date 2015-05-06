package gama
package intermediate

class ExprTransformTree {
  def transform(cmd: CmdHW): CmdHW = cmd match {
    case WireDecl(symbol, note)  => WireDecl(transform(symbol), note)
    case RegDecl(symbol, reset, note)   => RegDecl( transform(symbol), reset.map({
      case (ren, rval) => (transform(ren), transform(rval))
    }), note)
    case ConstDecl(symbol, expr, note) => ConstDecl(transform(symbol), transform(expr), note)
    case AliasDecl(symbol, ref, note)  => AliasDecl(transform(symbol), transform(ref), note )
    case BlockHW(stmts, note) => BlockHW(stmts.map(transform(_)), note)
    case WhenHW(cond, tc, fc, note) => WhenHW(transform(cond), transform(tc), transform(fc), note)

    case ConnectStmt(sink, source, details, note)  => ConnectStmt(transform(sink), transform(source), details, note)
    case BiConnectStmt(left, right, details, note) => BiConnectStmt(transform(left), transform(right), details, note)

    case MemDecl(_,_) | SubModuleDecl(_,_,_) => cmd
  }

  def transform(symbol: RefSymbol): RefSymbol = symbol
  def transform(ref: RefHW): RefHW  = ref match {
    case symbol: RefSymbol => (transform(symbol))

    case RefMSelect(memdesc, selector, note) => RefMSelect(memdesc, transform(selector), note)
    case RefVIndex(source, index, note)      => RefVIndex(transform(source), index, note)
    case RefVSelect(source, selector, note)  => RefVSelect(transform(source), transform(selector), note)
    case RefTLookup(source, field, note)     => RefTLookup(transform(source), field, note)
    case RefExtract(source, lp, rp, rType, note) => RefExtract(transform(source), lp, rp, rType, note)

    case RefIO(_,_) | RefExprERROR(_)  => ref
  }
  def transform(expr: ExprHW): ExprHW = expr match {
    case ref: RefHW => (transform(ref))

    case ExprUnary(op, target, rType, note)       => ExprUnary(op, transform(target), rType, note)
    case ExprBinary(op, left, right, rType, note) => ExprBinary(op, transform(left), transform(right), rType, note)
    case ExprMux(cond, tc, fc, rType, note) => ExprMux(transform(cond), transform(tc), transform(fc), rType, note)

    case ExprLit(_,_,_) | RefExprERROR(_) => expr
  }
}
