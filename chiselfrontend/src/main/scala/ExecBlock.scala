package gama
import internal._

object ExecBlock {
  def apply(work: =>Unit)(implicit em: EnclosingModule): Unit = new ExecBlock(work, em)
}
sealed class ExecBlock private (work: =>Unit, em: Module[_]) {
  private val journal = EmptyJournal()

  // Add the Conditionally onto the active journal
  em.getActiveJournal.append(AddExecBlock(journal))

  // Do work under the true condition journal
  em.pushJournal(journal)
  try {work}
  finally em.popJournal
}
