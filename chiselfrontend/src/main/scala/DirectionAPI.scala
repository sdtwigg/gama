package gama
import internal._

trait DirectionAPI {
  def Input[D<:Data](  in: D): in.type = { in.rebind(DirectionSpells.SetInput)  } 
  def Output[D<:Data]( in: D): in.type = { in.rebind(DirectionSpells.SetOutput) } 
  def Flipped[D<:Data](in: D): in.type = { in.rebind(DirectionSpells.Flip)      } 
}

