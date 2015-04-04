package gama
import internal._

object Direction {
  case object Input   { def apply[D<:Data](in: D): in.type = {in.rebind(DirectionSpells.SetInput)} }
  case object Output  { def apply[D<:Data](in: D): in.type = {in.rebind(DirectionSpells.SetOutput)} }
  case object Flipped { def apply[D<:Data](in: D): in.type = {in.rebind(DirectionSpells.Flip)} }
}

trait DirectionAPI {
  def Input   = Direction.Input
  def Output  = Direction.Output
  def Flipped = Direction.Flipped
}
