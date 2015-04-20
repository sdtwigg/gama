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

case object StructuralMimicException
  extends ChiselException(s"Internal mimic failure: Target could not mimic model due to structural discrepancies. (e.g. A Vec of differing lengths, Different HardwareTuple subfields or the same sufield referring to a different datatype in the copy compared to the original.)")
  // TODO: Better stack trace for this error

case object AmbiguousBiConnectException
  extends ChiselException("""
Cannot resolve bidirectional connection attempt. This is likely due to one of the following:
  1) Either left or right is NonConnectable or not Synthesizable (thus, bidirectional connection impossible).
  2) Neither left nor right is of type Port and thus no directional hints are available.
  3) Trying to connect an input to an input (or output to an output) of the same module and thus there is ambiguity.
  4) Similar to 3), but the associated ports are of different submodules within the same module.
  5) An improper cross-module connection is being attempted.
""")
