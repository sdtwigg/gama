package gama
package intermediate

class ExprTransformTree {
  def transform(cmd: CmdHW): CmdHW = cmd match {
    case WireDecl(symbol)  => WireDecl(transform(symbol))
    case RegDecl(symbol, reset)   => RegDecl( transform(symbol), reset.map({
      case (ren, rval) => (transform(ren), transform(rval))
    }))
    case ConstDecl(symbol, expr) => ConstDecl(transform(symbol), transform(expr))
    case AliasDecl(symbol, ref)  => AliasDecl(transform(symbol), transform(ref) )
    case BlockHW(stmts) => BlockHW(stmts.map(transform(_)))
    case WhenHW(cond, tc, fc) => WhenHW(transform(cond), transform(tc), transform(fc))

    case ConnectStmt(sink, source, details)  => ConnectStmt(transform(sink), transform(source), details)
    case BiConnectStmt(left, right, details) => BiConnectStmt(transform(left), transform(right), details)

    case MemDecl(_) | SubModuleDecl(_,_) => cmd
  }

  def transform(symbol: RefSymbol): RefSymbol = symbol
  def transform(ref: RefHW): RefHW  = ref match {
    case symbol: RefSymbol => (transform(symbol))

    case RefMSelect(memdesc, selector) => RefMSelect(memdesc, transform(selector))
    case RefVIndex(source, index)      => RefVIndex(transform(source), index)
    case RefVSelect(source, selector)  => RefVSelect(transform(source), transform(selector))
    case RefTLookup(source, field)     => RefTLookup(transform(source), field)
    case RefExtract(source, lp, rp, rType) => RefExtract(transform(source), lp, rp, rType)

    case RefIO(_) | RefExprERROR(_)  => ref
  }
  def transform(expr: ExprHW): ExprHW = expr match {
    case ref: RefHW => (transform(ref))

    case ExprUnary(op, target, rType)       => ExprUnary(op, transform(target), rType)
    case ExprBinary(op, left, right, rType) => ExprBinary(op, transform(left), transform(right), rType)
    case ExprMux(cond, tc, fc, rType) => ExprMux(transform(cond), transform(tc), transform(fc), rType)

    case ExprLit(_, _) | RefExprERROR(_) => expr
  }
}
