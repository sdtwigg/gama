package gama
package object api extends DirectionAPI {
  import internal._

  type AnyModule = Module[_<:Data]
  type AnyVec    = Vec[_<:Data]
}
