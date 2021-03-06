package gama
import internal._

object when {
  def apply(cond: Bool)(work: =>Unit)(implicit em: EnclosingModule): when = new when(cond, work, em.get)
}

sealed class when private (cond: Bool, work: =>Unit, em: Module[_]) {
  private val tcJournal = EmptyOpJournal()
  private val fcJournal = EmptyOpJournal()

  // Add the Conditionally onto the active journal
  em.getActiveJournal.append(Conditionally(cond.node, tcJournal, fcJournal))

  // Do work under the true condition journal
  em.pushJournal(tcJournal)
  try {work}
  finally em.popJournal

  def elsewhen(otherCond: Bool)(otherWork: =>Unit): when = {
    // Do the other work under the false condition journal inside a when with the other condition
    em.pushJournal(fcJournal)
    try { new when(otherCond, otherWork, em) }
    finally em.popJournal
  }

  def otherwise(otherWork: =>Unit): Unit = {
    // Do the other work under the false condition journal
    em.pushJournal(fcJournal)
    try {otherWork}
    finally em.popJournal
  }
}
