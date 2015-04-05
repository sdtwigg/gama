package gama
package internal

abstract class ChiselException(private val message: String, private val cause: Throwable=null) extends Exception(message, cause)
object ChiselException {
  def unapply(e: ChiselException): Option[(String,Throwable)] = Some((e.message, e.cause))
}

case class TraversalException(subfield: String, container: String, cause: Throwable)
  extends ChiselException(s"Exception while Traversing @ ${subfield} of ${container}:\n ${cause}", cause)
object TraversalException {
  def apply(work: =>Unit, subfield: String, container: String): Unit =
    try {work} catch {case e: ChiselException => (throw TraversalException(subfield, container, e))}
}

case class DesignLostException(lostLink: String)
  extends ChiselException(s"Parts of elaborated design Lost to Time and Space, likely due to garbase collection. Link lost at $lostLink. Consult developers guide for details.")
