package gama
package intermediate

trait ModuleTransformer {
  final def transform(target: ElaboratedModule): ElaboratedModule = 
    target.copy(body = transform(target.body))
  def transform(cmd: CmdHW): CmdHW
}

class CmdMultiTransformTree extends ModuleTransformer {
  // USE WHEN: Want to transform one command into multiple commands
  //   No inherent expression-level transform
  final def asOne(cmds: Iterable[CmdHW]): CmdHW =
    if(cmds.size == 1) cmds.head else BlockHW(cmds.toList, GamaNote.unknown) // Is this ok?
  def multiTransform(cmd: CmdHW): Iterable[CmdHW] = cmd match {
    case BlockHW(stmts, note) => Some( BlockHW(stmts.flatMap(multiTransform(_)), note) )
    case WhenHW(cond, tc, fc, note) => {
      Some(WhenHW(cond, asOne(multiTransform(tc)), asOne(multiTransform(fc)), note))
    }
    case WireDecl(_,_) | RegDecl(_,_,_) | ConstDecl(_,_,_) | AliasDecl(_,_,_) |
         ConnectStmt(_,_,_,_) | BiConnectStmt(_,_,_,_) |
         MemDecl(_,_) | MemRead(_,_,_,_,_) | MemWrite(_,_,_,_,_) |
         SubModuleDecl(_,_,_) | CmdERROR(_,_) => Some(cmd)
  }
  final def transform(cmd: CmdHW) = asOne(multiTransform(cmd))
}

class ExprTransformTreeFullSegregation extends ModuleTransformer {
  // USE WHEN: Want to transform ExprHW, RefHW, and RefSymbol in all contexts
  //   Note, 4 separate transform functions (for CmdHW, RegSymbol, RefHW, and ExprHW contexts)
  //   Will see most commands.
  def transform(cmd: CmdHW): CmdHW = cmd match {
    case WireDecl(symbol, note)  => WireDecl(transform(symbol), note)
    case RegDecl(symbol, reset, note)   => RegDecl( transform(symbol), reset.map({
      case (ren, rval) => (transform(ren), transform(rval))
    }), note)
    case ConstDecl(symbol, expr, note) => ConstDecl(transform(symbol), transform(expr), note)
    case AliasDecl(symbol, ref, note)  => AliasDecl(transform(symbol), transform(ref), note )
    case BlockHW(stmts, note) => BlockHW(stmts.map(transform(_)), note)
    case WhenHW(cond, tc, fc, note) => WhenHW(transform(cond), transform(tc), transform(fc), note)
    
    case MemRead(symbol, mem, address, en, note) => 
      MemRead(transform(symbol), mem, transform(address), transform(en), note)
    case MemWrite(mem, address, source, mask, note) =>
      MemWrite(mem, transform(address), transform(source), mask.map(transform(_)), note)

    case ConnectStmt(sink, source, details, note)  => ConnectStmt(transform(sink), transform(source), details, note)
    case BiConnectStmt(left, right, details, note) => BiConnectStmt(transform(left), transform(right), details, note)

    case MemDecl(_,_) | SubModuleDecl(_,_,_) | CmdERROR(_,_) => cmd
  }

  def transform(symbol: RefSymbol): RefSymbol = symbol
  def transform(ref: RefHW): RefHW  = ref match {
    case symbol: RefSymbol => (transform(symbol))

    case RefMSelect(memdesc, selector, note) => RefMSelect(memdesc, transform(selector), note)
    case RefVIndex(source, index, note)      => RefVIndex(transform(source), index, note)
    case RefVSelect(source, selector, note)  => RefVSelect(transform(source), transform(selector), note)
    case RefTLookup(source, field, note)     => RefTLookup(transform(source), field, note)
    case RefExtract(source, lp, rp, note)    => RefExtract(transform(source), lp, rp, note)

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

class ExprOnlyTransformTree extends ModuleTransformer {
  // USE WHEN: Only want to transform ExprHW (and RefHW, RefSymbol in ExprHW context)
  //   Will not see any commands without ExprHW context (like WireDecl, BiConnectStmt)
  def transform(cmd: CmdHW): CmdHW = cmd match {
    case RegDecl(symbol, reset, note)  => RegDecl( symbol, reset.map({
      case (ren, rval) => (transform(ren), transform(rval))
    }), note)
    case ConstDecl(symbol, expr, note) => ConstDecl(symbol, transform(expr), note)
    case BlockHW(stmts, note) => BlockHW(stmts.map(transform(_)), note)
    case WhenHW(cond, tc, fc, note) => WhenHW(transform(cond), transform(tc), transform(fc), note)
    
    case MemRead(symbol, mem, address, en, note) => 
      MemRead(symbol, mem, transform(address), transform(en), note)
    case MemWrite(mem, selector, source, mask, note) =>
      MemWrite(mem, transform(selector), transform(source), mask.map(transform(_)), note)

    case ConnectStmt(sink, source, details, note)  => ConnectStmt(sink, transform(source), details, note)
    
    // No expressions to transform here
    case WireDecl(symbol, note) => cmd 
    case AliasDecl(symbol, ref, note)  => cmd 
    case BiConnectStmt(left, right, details, note) => cmd
    case MemDecl(_,_) | SubModuleDecl(_,_,_) | CmdERROR(_,_) => cmd
  }

  def transform(expr: ExprHW): ExprHW = expr match {
    case ExprUnary(op, target, rType, note)       => ExprUnary(op, transform(target), rType, note)
    case ExprBinary(op, left, right, rType, note) => ExprBinary(op, transform(left), transform(right), rType, note)
    case ExprMux(cond, tc, fc, rType, note) => ExprMux(transform(cond), transform(tc), transform(fc), rType, note)
    case ExprLit(_,_,_) => expr

    case RefMSelect(memdesc, selector, note) => RefMSelect(memdesc, transform(selector), note)
    case RefVIndex(source, index, note)      => RefVIndex(transform(source), index, note)
    case RefVSelect(source, selector, note)  => RefVSelect(transform(source), transform(selector), note)
    case RefTLookup(source, field, note)     => RefTLookup(transform(source), field, note)
    case RefExtract(source, lp, rp, note)    => RefExtract(transform(source), lp, rp, note)

    case RefSymbol(_,_,_,_) | RefIO(_,_) | RefExprERROR(_) => expr
  }
}
