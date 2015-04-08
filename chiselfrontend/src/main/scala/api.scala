package gama
package object api
  extends DirectionAPI with LiteralBoolAPI
{
  import internal._

  type AnyModule = Module[_<:Data]
  type AnyVec    = Vec[_<:Data]

  object U extends LitUIntObjectImpl
  object S extends LitSIntObjectImpl
  object B extends LitBoolObjectImpl
  
  implicit class addUStoScalaInt(val target: Int) extends AnyVal {
    def U = api.U(target)
    def S = api.S(target)
  }
  implicit class addBtoScalaBool(val target: Boolean) extends AnyVal {
    def B = api.B(target)
  }
}
