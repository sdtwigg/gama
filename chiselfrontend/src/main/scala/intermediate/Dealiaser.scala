package gama
package intermediate

class Dealiaser {
  private val aliasTable = scala.collection.mutable.HashMap.empty[RefSymbol, RefHW]
  
  def addAlias(alias: RefSymbol, ref: RefHW): Unit = {
    aliasTable.update(alias, dealias(ref))
  }
  
  def dealias(expr: RefHW): RefHW = expr match {
    case rs @ RefSymbol(_, _, _) => aliasTable.getOrElse(rs,rs) // Key case

    case RefMSelect(mem, selector) => RefMSelect(mem, dealias(selector))
    case RefVIndex(source, index) => RefVIndex(dealias(source), index)
    case RefVSelect(source, selector) => RefVSelect(dealias(source), dealias(selector))
    case RefTLookup(source, field) => RefTLookup(dealias(source), field)
    case RefExtract(source, lp, rp, rt) => RefExtract(dealias(source), lp, rp, rt)
    case RefIO(_) | RefMSelect(_,_) | RefExprERROR(_) => expr
  }
  def dealias(expr: ExprHW): ExprHW = expr match {
    case ref: RefHW => (dealias(ref))

    case ExprUnary(op, target, rt) => ExprUnary(op, dealias(target), rt)
    case ExprBinary(op, left, right, rt) => ExprBinary(op, dealias(left), dealias(right), rt)
    case ExprMux(cond, tc, fc, rt) => ExprMux(dealias(cond), dealias(tc), dealias(fc), rt)

    case ExprLit(_,_) => expr
  }
}
