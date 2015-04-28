package gama
package internal
package journal

import frontend._

object JournalToFrontendIR {
  class RefTable(parent: Option[RefTable]) {
    private type TableEntry = Tuple2[RefHW, Boolean]
    private val table = scala.collection.mutable.HashMap.empty[Data,TableEntry]

    private[this] var lastsymbol = 0 // only the module-level RefTable should end up using this
    private def getFreshSymbol: Int = this.synchronized{
      parent.map(_.getFreshSymbol).getOrElse({lastsymbol=lastsymbol+1; lastsymbol})
    }
    
    def get(key: Data): Option[TableEntry] =
      table.get(key) orElse parent.flatMap(_.get(key))

    def add(key: Data, ref: RefHW, connectable: Boolean): Unit = {
      if(get(key).isDefined) { throw new Exception("Internal Error: Context Uniqueness Violation") }
      table(key) = (ref, connectable)
    }
    def addNewSymbol(in: Data, identifier: Option[String], connectable: Boolean): RefSymbol = {
      if(get(in).isDefined) { throw new Exception("Internal Error: Context Uniqueness Violation") }
      val newSymbol = RefSymbol(getFreshSymbol, identifier, constructType(in))
      table(in) = (newSymbol, connectable)
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

  def convertJournal(journal: Journal, parentref: Option[RefTable], parentexpr: Option[ExprTable]): BlockHW = 
  {
    val reftable = new RefTable(parentref)
    val exprtable = new ExprTable(parentexpr)
    
    val statements: List[CmdHW] = journal.entries.flatMap((entry: Entry) => (
      entry match {
        // Symbol creators
        case CreateOp(opdesc) => {
          val expr = convertExpr(opdesc, reftable, exprtable)
          if(opdesc.retVal.name.isDefined) {
            Some(ConstDecl(reftable.addNewSymbol(opdesc.retVal, extractName(opdesc), false), expr))
          } else {
            exprtable.add(opdesc.retVal, expr)
            None
          }
        }
        
        case CreateWire(wiredesc) => ???
        case CreateReg(regdesc) => ???
        case CreateAccessor(accdesc) => ???
        case CreateExtract(extdesc) => ???
        // Sort-of symbol creators 
        case CreateModule(module) => ???
        case CreateMem(mem) => ???
        // Control Flow
        case Conditionally(cond, tc, fc) => ???
        case AddBlock(code) => Some(convertJournal(code, Some(reftable), Some(exprtable)))
        // Connection operators
        case ConnectData(sink, source, details, info) => ???
        case BiConnectData(left, right, details, info) => ???
      })
    )
    BlockHW(statements)
  }

  def extractName(desc: Desc): Option[String] = desc.retVal.name match {
    case Some(NameTerm(identifier: String)) => Some(identifier)
    case _ => None // other NameTree terms shouldn't ever be possible for a retVal
    // TODO: CLEANUP
  }

  def convertExpr(opdesc: OpDesc, reftable: RefTable, exprtable: ExprTable): ExprHW = {
    def lookup(in: Data): ExprHW = in.name match {
      // TODO: this likely needs to be non-nested as other conversions will prob use parts
      // leaf types
      case None | Some(NameTerm(_)) =>
        (reftable.get(in).map(_._1) orElse exprtable.get(in)).getOrElse(RefExprERROR)
      case Some(NameLit(litdesc)) => ExprLit(litdesc.litMap.asLitTree, constructType(litdesc.retVal))
      // refinements
      case Some(NameIO(source)) => lookup(source.io)
      case Some(NameField(source, field)) => RefTLookup(lookup(source), field, constructType(source))
      case Some(NameIndex(source, index)) => RefVIndex(lookup(source), index, constructType(source))
      // error cases
      case Some(NameUNKNOWN) | Some(NameUnnamedOp(_)) => RefExprERROR
    }

    opdesc match {
      case UnaryOpDesc(op, input, rv, _)          => ExprUnary(op, lookup(input), constructType(rv))
      case BinaryOpDesc(op, (left, right), rv, _) => ExprBinary(op, lookup(left), lookup(right), constructType(rv))
      case MuxDesc(cond, tc, fc, rv, _)           => ExprMux(lookup(cond), lookup(tc), lookup(fc), constructType(rv))
    }
  }

  def constructType(model: Data): TypeHW = model match {
    // explicitely do not bother checking Port here
    case e: Element => (PrimitiveNode(e.node.storage))
    case v: Vec[_] => (VecHW(v.length, constructType(v.elemType)))
    case t: HardwareTuple => (TupleHW(t.subfields_ordered.map(
      {case (field: String, elem: Data) => (field, constructType(elem))}
    ).toVector))
  }
}
