package gama
package frontend
package api

import implementation.DirectionXFORM._

trait DirectionAPI {
  def Input[D<:Data](  in: D): D = toInput(in) 
  def Output[D<:Data]( in: D): D = toOutput(in)
  def Flipped[D<:Data](in: D): D = flip(in)
}

