package gama
package frontend

import implementation._

object block {
  def apply(work: =>Unit)(implicit em: EnclosingModule): Unit = new block(work, em)
}
sealed class block private (work: =>Unit, em: Module[_]) {
  private val subJournal = journal.Journal.empty()

  // Add the Conditionally onto the active journal
  em.getActiveJournal.append(journal.AddBlock(subJournal))

  // Do work under the true condition journal
  em.pushJournal(subJournal)
  try {work}
  finally em.popJournal
}
