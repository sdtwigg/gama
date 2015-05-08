package gama
package intermediate

import scala.collection.mutable.{HashMap=>HMap, HashSet=>HSet, ListBuffer=>ListB}

class AppendableRefSymbolTable(prescan: ElaboratedModule) {
  // Ignores scope
  private val seenSymbolIds = HSet.empty[Int]
  private var highestSymbolId = 0

  private type SymbolTableEntry = CreatesRefSymbol // For now, only store originating command for the symbol
  private val symbolTable = HMap.empty[RefSymbol,SymbolTableEntry]

  def addSymbol(creator: CreatesRefSymbol): Unit = {
    val symbol = creator.symbol
    if(symbolTable.isDefinedAt(symbol) || seenSymbolIds(symbol.symbol)) {
      throw new Exception(s"Malformed AST: $symbol with id ${symbol.symbol} already declared in module")
    }
    symbolTable.update(symbol, creator)
    seenSymbolIds += symbol.symbol
    highestSymbolId = math.max(highestSymbolId, symbol.symbol)
  }

  new ExprScanTree {
    override def scan(cmd: CmdHW) = cmd match {
      case creator: CreatesRefSymbol => (addSymbol(creator))
      case _ => super.scan(cmd) // Don't care that creator scan short-circuited since only looking at cmds
    }
  }.scan(prescan.body)

  // Expected client: Just supply CreatesRefSymbol (CmdHW) tree with hole where symbol id needed
  def grantNewSymbol(builder: Int=>CreatesRefSymbol): CreatesRefSymbol = {
    val newCmd = builder(highestSymbolId+1)
    addSymbol(newCmd)
    newCmd
  }
}

class AppendableMemDescTable(prescan: ElaboratedModule) {
  // Ignores scope
  private val seenMemIds = HSet.empty[Int]
  private var highestMemId = 0

  private type MemTableEntry = MemDecl // For now, only store originating command for the symbol
  private val memDescTable = HMap.empty[MemDesc, MemTableEntry]

  def addMem(creator: MemDecl): Unit = {
    val desc = creator.desc
    if(memDescTable.isDefinedAt(desc) || seenMemIds(desc.memid)) {
      throw new Exception(s"Malformed AST: $desc with id ${desc.memid} already declared in module")
    }
    memDescTable.update(desc, creator)
    seenMemIds += desc.memid
    highestMemId = math.max(highestMemId, desc.memid)
  }

  new ExprScanTree {
    override def scan(cmd: CmdHW) = cmd match {
      case creator @ MemDecl(_,_) => (addMem(creator))
      case _ => super.scan(cmd) // Don't care that creator scan short-circuited since only looking at cmds
    }
  }.scan(prescan.body)

  // Expected client: Just supply CreatesRefMem (CmdHW) tree with hole where memdesc id needed
  def grantNewMem(builder: Int=>MemDecl): MemDecl = {
    val newCmd = builder(highestMemId+1)
    addMem(newCmd)
    newCmd
  }
}
