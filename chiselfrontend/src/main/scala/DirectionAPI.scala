package gama
import internal._

trait DirectionAPI {
  def Input[D<:Data](  in: D): D = { in.copy.rebind(DirectionSpells.SetInput)  } 
  def Output[D<:Data]( in: D): D = { in.copy.rebind(DirectionSpells.SetOutput) } 
  def Flipped[D<:Data](in: D): D = { in.copy.rebind(DirectionSpells.Flip)      } 

  // TODO: CONSIDER: should these copy in first or just rebind it?
}

