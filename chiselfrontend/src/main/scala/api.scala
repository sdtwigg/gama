package gama
package object api extends DirectionAPI with LiteralBoolAPI {
  import internal._

  type AnyModule = Module[_<:Data]
  type AnyVec    = Vec[_<:Data]

  object U extends LitUIntObjectImpl
  object S extends LitSIntObjectImpl
}
