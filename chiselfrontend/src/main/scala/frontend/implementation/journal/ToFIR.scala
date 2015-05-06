package gama
package frontend
package implementation
package journal

object ToFIR {
  def processIO[MR<:ModuleRef](target: Module[_<:Data], toModuleRef: TypeHW=>MR, reftable: RefTable): MR = {
    val full_io_type = TupleHW(target.full_io.toMap.map({ case (field, elem) => (field, constructType(elem)) }))
    val moduleRef = toModuleRef(full_io_type)

    target.full_io.foreach({
      case (field, elem) => reftable.add(elem, (RefTLookup(RefIO(moduleRef, GamaNote()), field, GamaNote()), true))
    })

    moduleRef
  }

  def convertJournal(journal: Journal, parentref: Option[RefTable], parentexpr: Option[ExprTable],
                                       parentmem: Option[MemTable], parentmod: Option[SubModTable]): BlockHW = 
  {
    implicit val reftable  = new RefTable(parentref)
    implicit val exprtable = new ExprTable(parentexpr)
    implicit val memtable  = new MemTable(parentmem)
    implicit val modtable  = new SubModTable(parentmod)

    def hasName(in: Nameable): Boolean = in.name match {
      case None | Some(NameUNKNOWN) => false
      case Some(_) => true
    }
    def handleAliasCandidate(rv: Data, alias: RefHW, connectable: Boolean, note: GamaNote): Option[CmdHW] = {
      if(hasName(rv)) {
        val newref =
          if(connectable) AliasDecl(reftable.addNewSymbol(rv, extractName(rv), true, note), alias, note)
          else            ConstDecl(reftable.addNewSymbol(rv, extractName(rv), false, note), alias, note)
        Some(newref)
      }
      else {
        reftable.add(rv, (alias, connectable))
        None
      }
    }
    
    val statements: List[CmdHW] = journal.entries.flatMap((entry: Entry) => (
      entry match {
        // Symbol creators
        case CreateOp(opdesc) => {
          val expr = convertOp(opdesc, reftable, exprtable)
          val note = GamaNote(opdesc.info.debug)
          if(hasName(opdesc.retVal)) {
            Some(ConstDecl(reftable.addNewSymbol(opdesc.retVal, extractName(opdesc), false, note), expr, note))
          } else {
            exprtable.add(opdesc.retVal, expr)
            None
          }
        }
        
        case CreateWire(wiredesc) => {
          val note = GamaNote(wiredesc.info.debug)
          Some(WireDecl(reftable.addNewSymbol(wiredesc.retVal, extractName(wiredesc), true, note), note))
        }
        case CreateReg(regdesc) => {
          val note = GamaNote(regdesc.info.debug)
          val reset: Option[Tuple2[ExprHW, ExprHW]] = regdesc.reset.map({
            case (ren, rval) => (exprLookup(ren)._1, exprLookup(rval)._1)
          })
          Some(RegDecl(reftable.addNewSymbol(regdesc.retVal, extractName(regdesc), true, note), reset, note))
        }

        case CreateAccessor(accdesc) => {
          val note = GamaNote(accdesc.info.debug)
          val (newref: RefHW, connectable: Boolean) = accdesc.accRef match {
            case va: VecAccessible[_] => {
              val (srcexpr, connectable) = exprLookup(va.collection)
              (RefVSelect(srcexpr, exprLookup(accdesc.selector)._1, note), connectable)
            }
            case ma: MemAccessible[_] =>
              val nref = memtable.get(ma.collection).map(memdesc =>
                RefMSelect(memdesc, exprLookup(accdesc.selector)._1, note)
              ).getOrElse(RefExprERROR("MemLookup failed"))
              (nref, true)
          }
          handleAliasCandidate(accdesc.retVal, newref, connectable, note)
        }
        case CreateExtract(extdesc) => {
          val note = GamaNote(extdesc.info.debug)
          val (srcexpr, connectable) = exprLookup(extdesc.base)
          val newref = RefExtract(srcexpr, extdesc.left_pos, extdesc.right_pos, constructType(extdesc.retVal), note)
          handleAliasCandidate(extdesc.retVal, newref, connectable, note)
        }

        // Sort-of symbol creators 
        case CreateModule(module) => {
          val modref = modtable.addNewMod(module, reftable)
          Some(SubModuleDecl(modref, module.getClass.getName, GamaNote())) // TODO: Add note
        }
        case CreateMem(mem) => Some(MemDecl(memtable.addNewMem(mem), GamaNote(mem.info.debug)))
        // Control Flow
        case Conditionally(cond, tc, fc) =>
          Some(WhenHW(exprLookup(cond)._1,
            convertJournal(tc, Some(reftable), Some(exprtable), Some(memtable), Some(modtable)), 
            convertJournal(fc, Some(reftable), Some(exprtable), Some(memtable), Some(modtable)),
            GamaNote() // TODO: Add note
          ))
        case AddBlock(code) => Some( convertJournal(code, Some(reftable), Some(exprtable), Some(memtable), Some(modtable)) )
        // Connection operators
        case ConnectData(Sink(sink), Source(source), details, info) =>
          Some( ConnectStmt(refLookup(sink)._1, exprLookup(source)._1, details, GamaNote(info.debug)) )
        case BiConnectData(Left(left), Right(right), details, info) =>
          Some( BiConnectStmt(refLookup(left)._1, refLookup(right)._1, details, GamaNote(info.debug)) )
      }): Iterable[CmdHW]
    )
    BlockHW(statements, GamaNote()) // TODO: Add note
  }

  def extractName(desc: Desc): Option[String] = extractName(desc.retVal)
  def extractName(target: Nameable): Option[String] = target.name match {
    case Some(NameTerm(identifier: String)) => Some(identifier)
    case _ => None // other NameTree terms shouldn't ever be possible for a retVal
    // TODO: CLEANUP
  }

  // return expression and whether it is connectable
  def exprLookup(in: Data)(implicit reftable: RefTable, exprtable: ExprTable): Tuple2[ExprHW,Boolean] =
    in.name match {
      // Name at leaf 
      case Some(NameTerm(_)) | Some(NameIO(_,_)) | Some(NameUNKNOWN) =>
        ( reftable.get(in) orElse (exprtable.get(in).map((_,false))) ).getOrElse(
          (RefExprERROR("ExprLookup failed"), false)
        )
      case Some(NameLit(litdesc)) => (ExprLit(litdesc.litMap.asLitTree, constructType(litdesc.retVal), GamaNote()), false) // TODO: OK not to have note?
      // refinements
      case Some(NameField(_, _)) => refLookup(in)
      case Some(NameIndex(_, _)) => refLookup(in)
      // error cases
      case None => (RefExprERROR("Name Unknown (Contract failure)"), false) // at this stage, name should be known...
    }
  def refLookup(in: Data)(implicit reftable: RefTable, exprtable: ExprTable): Tuple2[RefHW,Boolean] = 
    reftable.get(in) getOrElse {in.name match {
      // refinements - Note: can reference expressions so call exprLookup!
      case Some(NameField(source, field)) => {
        val srcexpr = exprLookup(source)
        val newref = (RefTLookup(srcexpr._1, field, GamaNote()), srcexpr._2)
        reftable.add(in, newref)
        newref
      }
      case Some(NameIndex(source, index)) => {
        val srcexpr = exprLookup(source)
        val newref = (RefVIndex(srcexpr._1, index, GamaNote()), srcexpr._2)
        reftable.add(in, newref)
        newref
      }
      // error cases
      case Some(NameTerm(_)) | Some(NameIO(_,_)) | Some(NameUNKNOWN) | Some(NameLit(_)) | None =>
        (RefExprERROR("RefLookup failed"), false)
        // NameTerm/NameIO/NameUNKNOWN -> should have resolved by initial reftable get
        // NameLit -> references can't be directly formed from literals
        // None -> All synthesizable nodes should be named by now
    }}

  def convertOp(opdesc: OpDesc, reftable: RefTable, exprtable: ExprTable): ExprHW = {
    def lookup(in: Data): ExprHW = (exprLookup(in)(reftable, exprtable))._1
    opdesc match {
      case UnaryOpDesc(op, input, rv, info)          => ExprUnary(op, lookup(input), constructType(rv), GamaNote(info.debug))
      case BinaryOpDesc(op, (left, right), rv, info) => ExprBinary(op, lookup(left), lookup(right), constructType(rv), GamaNote(info.debug))
      case MuxDesc(cond, tc, fc, rv, info)           => ExprMux(lookup(cond), lookup(tc), lookup(fc), constructType(rv), GamaNote(info.debug))
    }
  }

  // TODO: use HashMap[TypeHW,TypeHW] so redundant types not created? saves memory....
  def constructType(model: Data): TypeHW = model match {
    // explicitely do not bother checking Port here
    case e: Element => (e.node.resolveDirection match {
      case Some(dir) => PrimitivePort(e.node.storage, dir)
      case None      => PrimitiveNode(e.node.storage)
    })
    case v: Vec[_] => (VecHW(v.length, constructType(v.elemType)))
    case t: HardwareTuple => (TupleHW(t.subfields.map(
      {case (field: String, elem: Data) => (field, constructType(elem))}
    )))
  }
}
