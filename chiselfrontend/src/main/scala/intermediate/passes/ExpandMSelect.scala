package gama
package intermediate
package passes

object ExpandMSelect extends PassMerger(Vector(ExpandMSelectSink, ExpandMSelectSource))

object ExpandMSelectSink extends GamaPass {
  val name = "ExpandMSelectSink"
  // This pass converts all instances of having a RefMSelect as a sink into MemWrite
  // TODO: Must be run after SubstituteAliases, ExplodeConnect, and DeclAggregateExplode
  //   (no AliasDecl or BiConnectStmt or sub-referenced aggregate mems)
  // Note, may still have a RefExtract to contend with which turns into a masked write
  def transform(target: ElaboratedModule): ElaboratedModule = {
    object Transformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW) = cmd match {
        case c @ ConnectStmt(sink, source, _, cnote) => sink match {
          case RefMSelect(mem, address, mnote) => Some( MemWrite(mem, address, source, None, mnote) )
          case RefExtract(RefMSelect(mem, address, mnote), lp, rp, _) => {
            val mask = ExprLitU( (BigInt(2) << lp) - (BigInt(1) << rp) )
            Some( MemWrite(mem, address, source, Some(mask), mnote) )
          }
          // TODO: Are these 2 cases sufficient?
          case _ => Some(c) 
        }

        case BiConnectStmt(_,_,_,note) => Some( CmdERROR(s"BiConnect found during $name", note) )
        case AliasDecl(_,_,note) => Some( CmdERROR(s"AliasDecl found during $name", note) )
        case _ => super.multiTransform(cmd)
      }
    }

    Transformer.transform(target)
  }
}

object ExpandMSelectSource extends GamaPass {
  val name = "ExpandMSelectSource"
  // This pass converts all instances of having a RefMSelect as a source into MemRead
  // TODO: Requires RemoveAliases and ExplodeConnects to have been run
  // TODO: Create Mux tree instead of Mux chain

  def transform(target: ElaboratedModule): ElaboratedModule = {
    val symbolGen = new AppendableRefSymbolTable(target)

    def extractMemRead(target: ExprHW): Tuple2[ExprHW, Seq[CmdHW]] = target match {
      case ExprLit(_,_,_) | RefSymbol(_,_,_,_) | RefIO(_,_) => (target, Seq.empty)

      case RefMSelect(mem, address, note) => {
        val (newaddress, addrCmds) = extractMemRead(address)
        val newRead = symbolGen.grantNewSymbol(symid =>
          MemRead(RefSymbol(symid, None, mem.mType, note), mem, newaddress, ExprLitB(true), note)
        )
        (newRead.symbol, addrCmds :+ newRead)
      }

      case expr @ ExprUnary(_, target, _, _) => {
        val (newtarget, targetCmds) = extractMemRead(target)
        (expr.copy(target=newtarget), targetCmds)
      }
      case expr @ ExprBinary(_, left, right, _, _) => {
        val (newleft,  leftCmds)  = extractMemRead(left)
        val (newright, rightCmds) = extractMemRead(right)
        (expr.copy(left=newleft, right=newright), leftCmds ++ rightCmds)
      }
      case expr @ ExprMux(cond, tc, fc, _, _) => {
        val (newcond, condCmds) = extractMemRead(cond)
        val (newtc, tcCmds) = extractMemRead(tc)
        val (newfc, fcCmds) = extractMemRead(fc)
        (expr.copy(cond=newcond, tc=newtc, fc=newfc), condCmds ++ tcCmds ++ fcCmds)
      }

      case RefVIndex(source, index, note) => {
        val (newsource,  sourceCmds)  = extractMemRead(source)
        (RefVIndex(newsource, index, note), sourceCmds)
      }
      case RefVSelect(source, selector, note) => {
        val (newsource,  sourceCmds)  = extractMemRead(source)
        val (newselector, selectorCmds) = extractMemRead(selector)
        (RefVSelect(newsource, newselector, note), sourceCmds ++ selectorCmds)
      }
      case RefTLookup(source, field, note) => {
        val (newsource,  sourceCmds)  = extractMemRead(source)
        (RefTLookup(newsource, field, note), sourceCmds)
      }
      case RefExtract(source, lp, rp, note) => {
        val (newsource,  sourceCmds)  = extractMemRead(source)
        (RefExtract(newsource, lp, rp, note), sourceCmds)
      }

      case RefExprERROR(_) => (target, Seq.empty)
    }

    // TODO: can probably abstract part of this into more generalized object
    object Transformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW) = cmd match {
        // These have no expressions to transform
        case WireDecl(_,_) | RegDecl(_, None, _) | BlockHW(_,_) | MemDecl(_,_) |
             SubModuleDecl(_,_,_) | CmdERROR(_,_) => super.multiTransform(cmd)
        // These do
        case RegDecl(symbol, Some((ren, rval)), note) => {
          val (newren,  renCmds) = extractMemRead(ren)
          val (newrval, rvalCmds) = extractMemRead(rval)
          renCmds ++ rvalCmds :+ RegDecl(symbol, Some((newren, newrval)), note)
        }
        case ConstDecl(symbol, expr, note) => {
          val (newexpr, exprCmds) = extractMemRead(expr)
          exprCmds :+ ConstDecl(symbol, newexpr, note)
        }
        case WhenHW(cond, tc, fc, note) => {
          val (newcond, condCmds) = extractMemRead(cond)
          condCmds :+ WhenHW(newcond, super.transform(tc), super.transform(fc), note)
        }
        case ConnectStmt(sink, source, details, note) => {
          val (newsource, sourceCmds) = extractMemRead(source)
          sourceCmds :+ ConnectStmt(sink, newsource, details, note)
        }
        case MemRead(symbol, desc, address, en, note) => {
          val (newaddress, addressCmds) = extractMemRead(address)
          val (newen, enCmds) = extractMemRead(en)
          addressCmds ++ enCmds :+ MemRead(symbol, desc, newaddress, newen, note)
        }
        case MemWrite(desc, address, source, mask, note) => {
          val (newaddress, addressCmds) = extractMemRead(address)
          val (newsource, sourceCmds) = extractMemRead(source)
          val (newmask, maskCmds) = mask.map(mask => {
            val (newmask, maskCmds) = extractMemRead(mask)
            (Some(newmask), maskCmds)
          }).getOrElse((None, Seq.empty))
          addressCmds ++ sourceCmds ++ maskCmds :+ MemWrite(desc, newaddress, newsource, newmask, note)
        }
        
        case BiConnectStmt(_,_,_,note) => Some( CmdERROR(s"BiConnect found during $name", note) )
        case AliasDecl(_,_,note) => Some( CmdERROR(s"AliasDecl found during $name", note) )
      }
    }

    Transformer.transform(target)
  }
}

