package gama

import internal._
case class EnclosingModule(val enclosed: Module[_<:Data])
object EnclosingModule {
  import scala.language.implicitConversions
  implicit def EM2M(in: EnclosingModule): Module[_<:Data] = in.enclosed
}

case object UnwrappedModuleException extends
  ChiselException("Module instantiations not properly wrapped in Module() call")
case object OverwrappedModuleException extends
  ChiselException("Module() improperly called inside other Module() call without intervening module creation.")

abstract class Module[+IOT<:Data](makeIO: IOT) {
  // Setup parent then add self to module stack
  val parent: Option[Module[_<:Data]] = Module.currentModule
  Module.push(this)

  // Capture operations on nodes inside this module
  implicit val __enclosingmodule = EnclosingModule(this)

  // First, must setup journal so can use them
  private val mainJournal = EmptyJournal()
  private val subJournalStack = scala.collection.mutable.Stack.empty[Journal]
    // sub-journals are used by when statements

  def getActiveJournal: Journal = subJournalStack.headOption.getOrElse(mainJournal)
  protected[gama] def pushJournal(in: Journal): Unit = {subJournalStack.push(in)}
  protected[gama] def popJournal: Journal = {subJournalStack.pop}
  
  // Now, module enclosing and journal setup complete so can construct the IO
  final val io: IOT =
    InternalName(Port(makeIO, __enclosingmodule), "io", NameFromIO)
  
  // Also, add self to parent, if it exists
  parent.foreach(_.addSubmodule(this))
  private def addSubmodule(child: Module[_<:Data]) = {
    getActiveJournal.append(CreateModule(child))
  }
}
object Module {
  private def currentModule: Option[Module[_<:Data]] = modStack.headOption
  private[this] var modWrapped: Boolean = false
  private[this] val modStack = scala.collection.mutable.Stack.empty[Module[_<:Data]]
  
  def apply[M<:Module[_<:Data]](in: =>M): M  = {
    if(modWrapped) {throw OverwrappedModuleException}
    modWrapped = true
    val created = in //push() called now
    modStack.pop()
    created
  }
  private def push(in: Module[_<:Data]): Unit = {
    if(!modWrapped) {throw UnwrappedModuleException}
    modWrapped = false
    modStack.push(in)
  }
}

