package gama
import internal._

object block {
  def apply(work: =>Unit)(implicit em: EnclosingModule): Unit = new block(work, em)
}
sealed class block private (work: =>Unit, em: Module[_]) {
  private val journal = EmptyJournal()

  // Add the Conditionally onto the active journal
  em.getActiveJournal.append(AddBlock(journal))

  // Do work under the true condition journal
  em.pushJournal(journal)
  try {work}
  finally em.popJournal
}
