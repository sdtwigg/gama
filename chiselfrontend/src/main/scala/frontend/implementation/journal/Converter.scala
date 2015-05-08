package gama
package frontend
package implementation
package journal

object Converter {
  type ModulePtrLUT = Map[Module[_<:Data],Int]
  def apply(topModule: Module[_<:Data]): ElaboratedCircuit = {
    val setupQueue = scala.collection.mutable.Queue[Module[_<:Data]](topModule)
    val moduleList = scala.collection.mutable.ListBuffer.empty[Module[_<:Data]]
    while(setupQueue.nonEmpty) {
      val checking = setupQueue.dequeue
      setupQueue.enqueue(checking.children:_*)
      moduleList.append(checking)
    }
    val moduleRoster = moduleList.toVector
    val modulePtrLUT: ModulePtrLUT = moduleRoster.zipWithIndex.toMap

    ElaboratedCircuit(moduleRoster.map(convertModule(_, modulePtrLUT)))
  }
  def convertModule(module: Module[_<:Data], modulePtrLUT: ModulePtrLUT): ElaboratedModule = {
    val reftable = new RefTable(None)
    val selfRef = ToFIR.processIO(module, (thw) => ModuleThis(thw), reftable)
    val body = ToFIR.convertJournal(module.getActiveJournal, Some(reftable), None, None, None)(modulePtrLUT)
    ElaboratedModule(module.getClass.getName, selfRef.ioType, body)
  }
}

