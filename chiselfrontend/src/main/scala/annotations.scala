package gama
package annotations

import internal._

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

