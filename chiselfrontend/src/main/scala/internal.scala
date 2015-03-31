package gama
package internal

abstract class ChiselException(private val message: String, private val cause: Throwable=null) extends Exception(message, cause)
object ChiselException {
  def unapply(e: ChiselException): Option[(String,Throwable)] = Some((e.message, e.cause))
}
