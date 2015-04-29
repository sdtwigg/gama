package gama
package internal
package journal

import frontend._

object JournalToFrontendIR {
  class MemTable(parent: Option[MemTable]) {
    private val table = scala.collection.mutable.HashMap.empty[Mem[_],MemDesc]
    private[this] var lastid = 0 // only the module-level MemTable should end up using this
    private def getFreshId: Int = this.synchronized{
      parent.map(_.getFreshId).getOrElse({lastid=lastid+1; lastid})
    }
    
    def get(key: Mem[_]): Option[MemDesc] = table.get(key) orElse parent.flatMap(_.get(key))
    def addNewMem(in: Mem[_<:Data]): MemDesc = {
      if(get(in).isDefined) { throw new Exception("Internal Error: Context Uniqueness Violation") }
      val newMem = MemDesc(getFreshId, extractName(in), in.depth, constructType(in.elemType))
      table(in) = newMem
      newMem
    }
  }
  class RefTable(parent: Option[RefTable]) {
    private val d2r_table = scala.collection.mutable.HashMap.empty[Data,RefHW]
    private val r2c_table = scala.collection.mutable.HashMap.empty[RefHW,Boolean] // connectable

    private[this] var lastsymbol = 0 // only the module-level RefTable should end up using this
    private def getFreshSymbol: Int = this.synchronized{
      parent.map(_.getFreshSymbol).getOrElse({lastsymbol=lastsymbol+1; lastsymbol})
    }
    
    def get(key: Data): Option[Tuple2[RefHW, Boolean]] =
      {for { // inner join
        ref <- d2r_table.get(key)
        connectable <- r2c_table.get(ref)
      } yield (ref, connectable)} orElse parent.flatMap(_.get(key))
    def get(ref: RefHW): Option[Boolean] = r2c_table.get(ref) orElse parent.flatMap(_.get(ref))

    def add(key: Data, value: Tuple2[RefHW, Boolean]): Unit = {
      if(get(key).isDefined) { throw new Exception("Internal Error: Context Uniqueness Violation") }
      d2r_table(key) = value._1
      r2c_table(value._1) = value._2
    }
    def addNewSymbol(in: Data, identifier: Option[String], connectable: Boolean): RefSymbol = {
      if(get(in).isDefined) { throw new Exception("Internal Error: Context Uniqueness Violation") }
      val newSymbol = RefSymbol(getFreshSymbol, identifier, constructType(in))
      add(in, (newSymbol, connectable))
      newSymbol
    }
  }
  class ExprTable(parent: Option[ExprTable]) {
    // an optimization, stores encountered unnamed expressions so they can be folded in later
    private type TableEntry = ExprHW
    private val table = scala.collection.mutable.HashMap.empty[Data,TableEntry]
    
    def get(key: Data): Option[TableEntry] =
      table.get(key) orElse parent.flatMap(_.get(key))

    def add(key: Data, expr: TableEntry): Unit = {
      if(get(key).isDefined) { throw new Exception("Internal Error: Context Uniqueness Violation") }
      table(key) = expr
    }
  }

  def convertJournal(journal: Journal, parentref: Option[RefTable], parentexpr: Option[ExprTable],
                                       parentmem: Option[MemTable]): BlockHW = 
  {
    implicit val reftable  = new RefTable(parentref)
    implicit val exprtable = new ExprTable(parentexpr)
    implicit val memtable  = new MemTable(parentmem)

    def hasName(in: Nameable): Boolean = in.name match {
      case None | Some(NameUNKNOWN) => false
      case Some(_) => true
    }
    
    val statements: List[CmdHW] = journal.entries.flatMap((entry: Entry) => (
      entry match {
        // Symbol creators
        case CreateOp(opdesc) => {
          val expr = convertOp(opdesc, reftable, exprtable)
          if(hasName(opdesc.retVal)) {
            Some(ConstDecl(reftable.addNewSymbol(opdesc.retVal, extractName(opdesc), false), expr))
          } else {
            exprtable.add(opdesc.retVal, expr)
            None
          }
        }
        
        case CreateWire(wiredesc) =>
          Some(WireDecl(reftable.addNewSymbol(wiredesc.retVal, extractName(wiredesc), true)))
        case CreateReg(regdesc) => 
          Some(RegDecl(reftable.addNewSymbol(regdesc.retVal, extractName(regdesc), true)))

        // TODO emit RefDecl when have name
        case CreateAccessor(accdesc) => {
          val (newref, connectable) = accdesc.accRef match {
            case va: VecAccessible[_] => {
              val (srcexpr, connectable) = exprLookup(va.collection)
              (RefVSelect(srcexpr, exprLookup(accdesc.selector)._1, constructType(accdesc.retVal)), connectable)
            }
            case ma: MemAccessible[_] =>
              val nref = memtable.get(ma.collection).map(memdesc =>
                RefMSelect(memdesc, exprLookup(accdesc.selector)._1)
              ).getOrElse(RefExprERROR("MemLookup failed"))
              (nref, true)
          }
          reftable.add(accdesc.retVal, (newref, connectable))
          None
        }
        case CreateExtract(extdesc) => {
          val (srcexpr, connectable) = exprLookup(extdesc.base)
          val newref = RefExtract(srcexpr, extdesc.left_pos, extdesc.right_pos, constructType(extdesc.retVal))
          reftable.add(extdesc.retVal, (newref, connectable))
          None
        }

        // Sort-of symbol creators 
        case CreateModule(module) => ???
        case CreateMem(mem) => Some(MemDecl(memtable.addNewMem(mem)))
        // Control Flow
        case Conditionally(cond, tc, fc) => ???
        case AddBlock(code) => Some(convertJournal(code, Some(reftable), Some(exprtable), Some(memtable)))
        // Connection operators
        case ConnectData(sink, source, details, info) => ???
        case BiConnectData(left, right, details, info) => ???
      })
    )
    BlockHW(statements)
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
      case Some(NameTerm(_)) | Some(NameIO(_)) | Some(NameUNKNOWN) =>
        ( reftable.get(in) orElse (exprtable.get(in).map((_,false))) ).getOrElse(
          (RefExprERROR("ExprLookup failed"), false)
        )
      case Some(NameLit(litdesc)) => (ExprLit(litdesc.litMap.asLitTree, constructType(litdesc.retVal)), false)
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
        val newref = (RefTLookup(srcexpr._1, field, constructType(source)), srcexpr._2)
        reftable.add(in, newref)
        newref
      }
      case Some(NameIndex(source, index)) => {
        val srcexpr = exprLookup(source)
        val newref = (RefVIndex(srcexpr._1, index, constructType(source)), srcexpr._2)
        reftable.add(in, newref)
        newref
      }
      // error cases
      case Some(NameTerm(_)) | Some(NameIO(_)) | Some(NameUNKNOWN) | Some(NameLit(_)) | None =>
        (RefExprERROR("RefLookup failed"), false)
        // NameTerm/NameIO/NameUNKNOWN -> should have resolved by initial reftable get
        // NameLit -> references can't be directly formed from literals
        // None -> All synthesizable nodes should be named by now
    }}

  def convertOp(opdesc: OpDesc, reftable: RefTable, exprtable: ExprTable): ExprHW = {
    def lookup(in: Data): ExprHW = (exprLookup(in)(reftable, exprtable))._1
    opdesc match {
      case UnaryOpDesc(op, input, rv, _)          => ExprUnary(op, lookup(input), constructType(rv))
      case BinaryOpDesc(op, (left, right), rv, _) => ExprBinary(op, lookup(left), lookup(right), constructType(rv))
      case MuxDesc(cond, tc, fc, rv, _)           => ExprMux(lookup(cond), lookup(tc), lookup(fc), constructType(rv))
    }
  }

  // TODO: use HashMap[TypeHW,TypeHW] so redundant types not created? saves memory....
  def constructType(model: Data): TypeHW = model match {
    // explicitely do not bother checking Port here
    case e: Element => (PrimitiveNode(e.node.storage))
    case v: Vec[_] => (VecHW(v.length, constructType(v.elemType)))
    case t: HardwareTuple => (TupleHW(t.subfields_ordered.map(
      {case (field: String, elem: Data) => (field, constructType(elem))}
    ).toVector))
  }
}
