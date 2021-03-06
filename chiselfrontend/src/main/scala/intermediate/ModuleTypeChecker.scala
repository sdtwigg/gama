package gama
package intermediate

class ModuleTypeChecker(warnWidthUnknown: Boolean)(target: ElaboratedModule) {
  // Helper classes and data structures
  case class LeafTableEntry(connectable: Boolean)
  class LeafTable(parent: Option[LeafTable]) {
    private val table = scala.collection.mutable.HashMap.empty[ExprLeaf, LeafTableEntry]

    def get(key: ExprLeaf): Option[LeafTableEntry] =
      table.get(key) orElse parent.flatMap(_.get(key))
    def add(key: ExprLeaf, value: LeafTableEntry): Unit = {
      table(key) = value
    }
  }

  case class Problem(loc: PathTrace, issue: String)
  val errors   = scala.collection.mutable.ListBuffer.empty[Problem]
  val warnings = scala.collection.mutable.ListBuffer.empty[Problem]
  
  // CHECK TO ENSURE PORT IS OK

  val ioRef = RefIO(ModuleThis(target.io), GamaNote(GTSourcePass(this.toString)))
  checkPort(ioRef.mod.ioType, PTStart(ioRef))

  val moduleLeafTable = new LeafTable(None)
  moduleLeafTable.add(ioRef, LeafTableEntry(true))

  checkCmdHW(target.body, moduleLeafTable)

  // All the checking functions below....
  def checkBlockHW(block: BlockHW, parentlt: LeafTable): Unit = {
    val leafTable = new LeafTable(Some(parentlt))
    block.statements.foreach(stmt => checkCmdHW(stmt, leafTable))
  }
  def checkCmdHW(cmd: CmdHW, leafTable: LeafTable): Unit = cmd match {
    case WireDecl(symbol, note)  => 
    case RegDecl(symbol, clock, reset, note) =>
    case ConstDecl(symbol, expr, note) =>
    case AliasDecl(symbol, ref, note) =>

    case b @ BlockHW(_,_) => checkBlockHW(b, leafTable)
    case WhenHW(cond, tc, fc, note) => {
      checkExpr(cond, leafTable)
      cond.rType match {
        case PrimitiveNode(UBits(Some(1))) => // OK
        case PrimitiveNode(UBits(None)) => if(warnWidthUnknown) {warnings+=Problem(PTStart(cond),"Width Unknown")}
        case _ => errors+=Problem(PTStart(cond),"WhenHW cond must be of type UBits(1|?)")
      }
      checkCmdHW(tc, leafTable)
      checkCmdHW(fc, leafTable)
    }

    case ConnectStmt(sink, source, details, note) => 
    case BiConnectStmt(left, right, details, note) => 
    
    case MemDecl(desc, clock, note) => 
    case MemRead(symbol, mem, address, en, note) => 
    case MemWrite(mem, address, source, mask, note) => 

    case SubModuleDecl(details, ph, note) => 
    case CmdERROR(message, note) =>
  }
  
  def checkNodeStore(check: NodeStore, path: PathTrace): Unit = check match {
    case rb: RawDigital => (if(warnWidthUnknown && rb.width.isEmpty) {
      warnings += Problem(path, "Width Unknown")
    })
    case ra: RawAnalog =>
  }
  def checkPort(check: TypeHW, path: PathTrace): Unit = check match {
    case PrimitivePort(storage,_) => checkNodeStore(storage, path)
    case PrimitiveNode(_) => errors += Problem(path, "IO Leaf is not PrimitivePort")
    case TypeHWUNKNOWN => errors += Problem(path, "IO Leaf type is malformed.")
    case TupleHW(fields) => fields.foreach( {case (field, elem) => checkPort(elem, PTField(path, field))} )
    case VecHW(_, elemt) => checkPort(elemt, PTSelectALL(path))
  }
  def checkNode(check: TypeHW, path: PathTrace): Unit = check match {
    case PrimitiveNode(storage) => checkNodeStore(storage, path)
    case PrimitivePort(_,_) => errors += Problem(path, "Node Leaf is cannot be a PrimitivePort")
    case TypeHWUNKNOWN => errors += Problem(path, "Node Leaf type is malformed.")
    case TupleHW(fields) => fields.foreach( {case (field, elem) => checkNode(elem, PTField(path, field))} )
    case VecHW(_, elemt) => checkNode(elemt, PTSelectALL(path))
  }

  def checkExpr(expr: ExprHW, lt: LeafTable): Unit = {
    checkNode(expr.rType, PTStart(expr))
    expr match {
      case _ => 
    }
  }
}
