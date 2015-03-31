package gama

import internal._

case class EnclosingModule(val em: Option[Module[_]])
object EnclosingModule {
  import scala.language.implicitConversions
  implicit def EM2M(in: EnclosingModule): Option[Module[_]] = in.em
}

abstract class Module[IOT<:Data](makeIO: IOT)(val parent: Option[Module[_]]) {
  // Capture operations on nodes inside this module
  implicit val __enclosingmodule = EnclosingModule(Some(this))

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

/*
  Plan is to expand class and object from:
    @gamamod class Test extends Module(UInt()) {
      ...
    }
  To:
    class TestModule protected (parent: Option[Module[_]]) extends Module(UInt())(parent) {
    object TestModule {
      def apply()(implicit parent: EnclosingModule) = new TestModule(parent.em)
    }
*/
class ExampleModule protected (parent: Option[Module[_]]) extends Module(UInt())(parent) {
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
  def apply()(implicit parent: EnclosingModule) = new ExampleModule(parent.em)
}
