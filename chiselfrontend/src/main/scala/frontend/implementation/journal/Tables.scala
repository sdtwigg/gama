package gama
package frontend
package implementation
package journal

class SubModTable(parent: Option[SubModTable]) {
  private val table = scala.collection.mutable.HashMap.empty[Module[_],ModuleSub]
  private[this] var lastid = 0 // only the module-level SubModTable should end up using this
  private def getFreshId: Int = this.synchronized{
    parent.map(_.getFreshId).getOrElse({lastid=lastid+1; lastid})
  }
  
  def get(key: Module[_]): Option[ModuleSub] = table.get(key) orElse parent.flatMap(_.get(key))
  def addNewMod(in: Module[_<:Data], reftable: RefTable): ModuleSub = {
    if(get(in).isDefined) { throw new Exception("Internal Error: Context Uniqueness Violation") }
    val newId = getFreshId
    val modRef = ToFIR.processIO(in, (thw) => ModuleSub(newId, ToFIR.extractName(in), thw), reftable)
    table(in) = modRef
    modRef
  }
}

class MemTable(parent: Option[MemTable]) {
  private val table = scala.collection.mutable.HashMap.empty[Mem[_],MemDesc]
  private[this] var lastid = 0 // only the module-level MemTable should end up using this
  private def getFreshId: Int = this.synchronized{
    parent.map(_.getFreshId).getOrElse({lastid=lastid+1; lastid})
  }
  
  def get(key: Mem[_]): Option[MemDesc] = table.get(key) orElse parent.flatMap(_.get(key))
  def addNewMem(in: Mem[_<:Data]): MemDesc = {
    if(get(in).isDefined) { throw new Exception("Internal Error: Context Uniqueness Violation") }
    val newMem = MemDesc(getFreshId, ToFIR.extractName(in), in.depth, ToFIR.constructType(in.elemType))
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
    val newSymbol = RefSymbol(getFreshSymbol, identifier, ToFIR.constructType(in))
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
