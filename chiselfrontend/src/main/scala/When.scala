package gama
import internal._

object when {
  def apply(cond: Bool)(work: =>Unit)(implicit em: EnclosingModule): when = new when(cond, work, em)
}

sealed class when private (cond: Bool, work: =>Unit, em: Module[_]) {
  private val tcJournal = EmptyJournal()
  private val fcJournal = EmptyJournal()

  // Add the Conditionally onto the active journal
  em.getActiveJournal.append(Conditionally(cond, tcJournal, fcJournal))

  // Do work under the true condition journal
  em.pushJournal(tcJournal)
  try {work}
  finally em.popJournal

  def elsewhen(otherCond: =>Bool)(otherWork: =>Unit): when = {
    // Do the other work under the false condition journal inside a when with the other condition
    //   otherCond is also call-by-name so that any created nodes contained within the false journal
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
