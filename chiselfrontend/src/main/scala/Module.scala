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
  val parent: Option[Module[_<:Data]] = Module.currentModule
  Module.push(this)

  // Capture operations on nodes inside this module
  implicit val __enclosingmodule = EnclosingModule(this)

  // First, must setup journal so can use them
  private val mainJournal = EmptyOpJournal()
  private val subJournalStack = scala.collection.mutable.Stack.empty[OpJournal]
    // sub-journals are used by when statements

  def getActiveJournal: OpJournal = subJournalStack.headOption.getOrElse(mainJournal)
  protected[gama] def pushJournal(in: OpJournal): Unit = {subJournalStack.push(in)}
  protected[gama] def popJournal: OpJournal = {subJournalStack.pop}
  
  // Now, module enclosing and journal setup complete so can construct the IO
  final val io: IOT = Port(makeIO)
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

class ExampleModule protected () extends Module(UInt()) {
  val uint1 = Wire(UInt())
  val uint2 = Wire(UInt())
  val select1 = Wire(Bool())
  val select2 = Wire(Bool())
  val select3 = Wire(Bool())

  val myMux = Mux(select1, uint1, uint2)
  val test = Wire(UInt())
  
  test := select1
/*
  when(select1) {
    when(select2) {
      test := select1
    }
    test := uint1
  }.elsewhen(select3) {
    test := select2
  } otherwise {
    test := uint2
  }
*/
}
object ExampleModule {
  def apply(): ExampleModule = Module(new ExampleModule)
}
