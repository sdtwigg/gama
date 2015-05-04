package gama
package frontend

import implementation._
import journal.{Journal} // journal from internal

final class EnclosingModule private (incomingEnclosed: Module[_<:Data]) extends EnclosureWeakReference[Module[_<:Data]](incomingEnclosed) {
  protected[this] val lostLinkDetails: String = s"EnclosingModule (previously connected to ${incomingEnclosed.toString})"
}
object EnclosingModule {
  def apply(enclosing: Module[_<:Data]) = new EnclosingModule(enclosing)
  import scala.language.implicitConversions
  implicit def EM2M(in: EnclosingModule): Module[_<:Data] = in.enclosure
}

case object UnwrappedModuleException extends
  ChiselException("Module instantiations not properly wrapped in Module() call")
case object OverwrappedModuleException extends
  ChiselException("Module() improperly called inside other Module() call without intervening module creation.")

abstract class Module[+IOT<:Data](makeIO: IOT) extends Nameable {
  // Setup parent then add self to module stack before an exception that leads to Module stack corruption
  protected[gama] val parent: Option[Module[_<:Data]] = Module.currentModule
  Module.push(this)

  // Capture operations on nodes inside this module
  implicit val __enclosingmodule = EnclosingModule(this)

  // First, must setup journal so can use them
  private[this] val mainJournal = Journal.empty()
  private[this] val subJournalStack = scala.collection.mutable.Stack.empty[Journal]
    // sub-journals are used by when statements

  def getActiveJournal: Journal = subJournalStack.headOption.getOrElse(mainJournal)
  protected[gama] def pushJournal(in: Journal): Unit = {subJournalStack.push(in)}
  protected[gama] def popJournal: Journal = {subJournalStack.pop}
  
  // Module enclosing and journal setup complete so can construct the IO
  // -> construct the general io containter
  protected[gama] val full_io = scala.collection.mutable.HashMap.empty[String,Data]
  protected[gama] def addIOPin(field: String, io: Data): Unit =
    full_io.update(field, io)

  // -> construct and add the part that the user wants to use
  protected[gama] val ioDesc: PortDesc[IOT] = PortDesc(Port(makeIO, __enclosingmodule), EnclosureInfo(__enclosingmodule, None))
  final def io: IOT = ioDesc.retVal
  io.setDescRef(ioDesc, true)
  io.forceSetName(NameIO(this,"io"), NameFromIO, true)
  addIOPin("io", io)
  
  // Add self to parent, if it exists
  parent.foreach(_.addSubmodule(this))
  private[this] var _children = scala.collection.mutable.ListBuffer.empty[Module[_<:Data]]
  protected[gama] def children = _children.toList
  private def addSubmodule(child: Module[_<:Data]) = {
    getActiveJournal.append(journal.CreateModule(child))
    _children.append(child)
  }

  // Add miscellaneous module utilities
  lazy private[this] val resetDesc: PortDesc[Bool] = {
    val rdesc = PortDesc(Port(DirectionXFORM.toInput(Bool()), __enclosingmodule),
                        EnclosureInfo(__enclosingmodule, None))
    val rval = rdesc.retVal
    rval.setDescRef(rdesc, true)
    rval.forceSetName(NameIO(this,"reset"), NameFromIO, true)
    addIOPin("reset", rval)
    // TODO: CONSIDER: What if some weirdness happens and reset is first asked for by submodule to parent when parent is in deeper scope
    //  Could happen if the user is doing wacky things with addIOPin and lazy vals as well.
    //  Likely resolution is for parent to save where the submodule was created and append the reset connect entry
    //    to that journal directly. Will fix later?
    parent.foreach(p => rval.doConnectTo(p.reset, EnclosureInfo(p.__enclosingmodule, None)))

    rdesc
  }
  def reset: Bool = resetDesc.retVal

  def propogateName(newname: NameTree, newsource: NameSource): Unit = {} // do not propogate to IO
}
object Module {
  private def currentModule: Option[Module[_<:Data]] = modStack.headOption
  private[this] var modWrapped: Boolean = false
  private[this] val modStack = scala.collection.mutable.Stack.empty[Module[_<:Data]]
  
  def apply[M<:Module[_<:Data]](in: =>M): M  = {
    if(modWrapped) {throw OverwrappedModuleException}
    modWrapped = true
    try{in} //push() called now
    finally {modStack.pop()}
  }
  private def push(in: Module[_<:Data]): Unit = {
    if(!modWrapped) {throw UnwrappedModuleException}
    modWrapped = false
    modStack.push(in)
  }
}

