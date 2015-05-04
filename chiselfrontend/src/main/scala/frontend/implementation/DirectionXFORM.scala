package gama
package frontend
package implementation

object DirectionXFORM {
  def toInput[D<:Data](  in: D): D = { in.copy.rebind(DirectionSpells.SetInput)  } 
  def toOutput[D<:Data]( in: D): D = { in.copy.rebind(DirectionSpells.SetOutput) } 
  def flip[D<:Data](in: D): D = { in.copy.rebind(DirectionSpells.Flip)      } 
  
  // TODO: CONSIDER: should these copy in first or just rebind it?
  // Should they insist in be SPEC as well?
}
