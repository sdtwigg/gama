package gama
package internal {

sealed trait DirectionIO
object DirectionIO {
  case object Input  extends DirectionIO
  case object Output extends DirectionIO
}

}
import internal._

object Direction {
  case object Input   { def apply[D<:Data](in: D): in.type = {in.rebind(DirectionSpells.SetInput)} }
  case object Output  { def apply[D<:Data](in: D): in.type = {in.rebind(DirectionSpells.SetOutput)} }
  case object Flipped { def apply[D<:Data](in: D): in.type = {in.rebind(DirectionSpells.Flip)} }
}

