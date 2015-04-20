package gama
package object api
  extends DirectionAPI with LiteralBoolAPI
  with HWDataTypesAPI with NodeAPI with ControlFlowAPI
{
  import internal._

  // Literals (implicit classes cannot be in separate API)
  object U extends LitUIntObjectImpl
  object S extends LitSIntObjectImpl
  object B extends LitBoolObjectImpl
  object LitVec extends LitVecObjectImpl
  
  implicit class addUStoScalaInt(val target: Int) extends AnyVal {
    def U = api.U(target)
    def S = api.S(target)
  }
  implicit class addBtoScalaBool(val target: Boolean) extends AnyVal {
    def B = api.B(target)
  }

  // Macro Annotations (cannot be in separate API)
  import scala.language.experimental.macros
  import scala.annotation.StaticAnnotation
  import scala.annotation.compileTimeOnly

  @compileTimeOnly("Enable macro paradise to expand macro annotations")
  class module extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro macroAnno.moduleimpl
  }
  @compileTimeOnly("Enable macro paradise to expand macro annotations")
  class bundle extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro macroAnno.bundleimpl
  }
  @compileTimeOnly("Enable macro paradise to expand macro annotations")
  class probe extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro macroAnno.probeimpl
  }

  object MACRODEBUG {
    def apply[T](target: T): T = macro macroDef.debug
  }
}
