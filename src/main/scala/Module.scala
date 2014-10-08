package gama

import internal._

case class EnclosingModule(val em: Option[Module[_]])
object EnclosingModule {
  import scala.language.implicitConversions
  implicit def EM2M(in: EnclosingModule): Option[Module[_]] = in.em
}

abstract class Module[IOT<:Data : Regenerate](makeIO: IOT)(val parent: Option[Module[_]]) {
  implicit val __enclosingmodule = EnclosingModule(Some(this))

  final val io: IOT = Port(makeIO)

  private val mainJournal = EmptyOpJournal()
  private val subJournalStack = scala.collection.mutable.Stack.empty[OpJournal]
    // sub-journals are used by when statements

  def getActiveJournal: OpJournal = subJournalStack.headOption.getOrElse(mainJournal)
}

/*
  Plan is to expand class and object from:
    @gamamod class Test extends Module(UInt()) {
      ...
    }
  To:
*/
class TestModule protected (parent: Option[Module[_]]) extends Module(UInt())(parent) {
  println(io.owner)
  println(implicitly[EnclosingModule])
  println(parent)
}
object TestModule {
  def apply()(implicit parent: EnclosingModule) = new TestModule(parent.em)
}