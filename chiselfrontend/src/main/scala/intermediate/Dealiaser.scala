package gama
package intermediate

class Dealiaser {
  private val aliasTable = scala.collection.mutable.HashMap.empty[RefSymbol, RefHW]
  
  def addAlias(alias: RefSymbol, ref: RefHW): Unit = {
    aliasTable.update(alias, dealias(ref))
  }
  
  def dealias(expr: RefHW): RefHW = expr match {
    case rs @ RefSymbol(_, _, _, _) => aliasTable.getOrElse(rs,rs) // Key case

    case RefMSelect(mem, selector, note)      => RefMSelect(mem, dealias(selector), note)
    case RefVIndex(source, index, note)       => RefVIndex(dealias(source), index, note)
    case RefVSelect(source, selector, note)   => RefVSelect(dealias(source), dealias(selector), note)
    case RefTLookup(source, field, note)      => RefTLookup(dealias(source), field, note)
    case RefExtract(source, lp, rp, rt, note) => RefExtract(dealias(source), lp, rp, rt, note)
    case RefIO(_,_) | RefMSelect(_,_,_) | RefExprERROR(_) => expr
  }
  def dealias(expr: ExprHW): ExprHW = expr match {
    case ref: RefHW => (dealias(ref))

    case ExprUnary(op, target, rt, note) => ExprUnary(op, dealias(target), rt, note)
    case ExprBinary(op, left, right, rt, note) => ExprBinary(op, dealias(left), dealias(right), rt, note)
    case ExprMux(cond, tc, fc, rt, note) => ExprMux(dealias(cond), dealias(tc), dealias(fc), rt, note)

    case ExprLit(_,_,_) => expr
  }
}
