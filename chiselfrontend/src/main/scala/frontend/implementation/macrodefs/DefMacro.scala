package gama
package frontend
package implementation
package macrodefs

protected[gama] object DefMacro {
  import scala.reflect.macros.blackbox.Context

  def debug(c: Context)(target: c.Tree): c.Tree = {
    import c.universe._
    println(showCode(target))
    target
  }
}

