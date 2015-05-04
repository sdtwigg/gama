package gama
package frontend
package object api
  extends DirectionAPI with LiteralAPI
  with HWDataTypesAPI with NodeAPI with ControlFlowAPI
{
  // More Literals (implicit classes cannot be in separate API)
  implicit class addUStoScalaInt(val target: Int) extends AnyVal {
    def U = LiteralUInt(target)
    def S = LiteralSInt(target)
  }
  implicit class addBtoScalaBool(val target: Boolean) extends AnyVal {
    def B = LiteralBool(target)
  }

  // Macro Annotations (cannot be in separate API)
  import scala.language.experimental.macros
  import scala.annotation.StaticAnnotation
  import scala.annotation.compileTimeOnly

  import implementation.macrodefs._

  @compileTimeOnly("Enable macro paradise to expand macro annotations")
  class module extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro AnnoImpl.moduleimpl
  }
  @compileTimeOnly("Enable macro paradise to expand macro annotations")
  class bundle extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro AnnoImpl.bundleimpl
  }
  @compileTimeOnly("Enable macro paradise to expand macro annotations")
  class probe extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro AnnoImpl.probeimpl
  }

  object MACRODEBUG {
    def apply[T](target: T): T = macro DefMacro.debug
  }
}
