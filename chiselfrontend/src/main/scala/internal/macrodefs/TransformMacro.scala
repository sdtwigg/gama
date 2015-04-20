package gama
package internal
package macrodefs

protected[gama] object TransformMacro {
  import scala.reflect.macros.blackbox.Context

  def to_apply1(c: Context)(arg0: c.Tree): c.Tree = {
    import c.universe._
    val myThis = c.prefix.tree
    val emtype = tq"_root_.gama.EnclosingModule"
    q"$myThis.apply($arg0, implicitly[$emtype])"
  }
  def to_apply2(c: Context)(arg0: c.Tree, arg1: c.Tree): c.Tree = {
    import c.universe._
    val myThis = c.prefix.tree
    val emtype = tq"_root_.gama.EnclosingModule"
    q"$myThis.apply($arg0, $arg1, implicitly[$emtype])"
  }
}

