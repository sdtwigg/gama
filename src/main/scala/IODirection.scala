package gama

trait IODirection
object INPUT  extends IODirection
object OUTPUT extends IODirection

object IOHelper {
  // Helper function so that datatype construction objects can accept directions
  //    and return appropriately directioned nodes
  def applyIO[T<:Data](target: T, dir: IODirection): T = dir match {
    case INPUT  => target.asInput
    case OUTPUT => target.asOutput
  }
}
