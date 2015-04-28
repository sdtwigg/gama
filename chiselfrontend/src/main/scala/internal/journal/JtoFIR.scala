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
  private type TableEntry = ExprHW
  class ExprTable(parent: Option[ExprTable]) {
    // an optimization, stores encountered unnamed expressions so they can be folded in later
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
        case CreateOp(opdesc) => ???
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

  def convertExpr(result: Data): ExprHW = ??? // will need to do some folding

  def constructType(model: Data): TypeHW = model match {
    // explicitely do not bother checking Port here
    case e: Element => (PrimitiveNode(e.node.storage))
    case v: Vec[_] => (VecHW(v.length, constructType(v.elemType)))
    case t: HardwareTuple => (TupleHW(t.subfields_ordered.map(
      {case (field: String, elem: Data) => (field, constructType(elem))}
    ).toVector))
  }
}
