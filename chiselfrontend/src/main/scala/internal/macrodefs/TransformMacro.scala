package gama
package internal
package macrodefs

protected[gama] object TransformMacro {
  import scala.reflect.macros.blackbox.Context

  sealed trait CoreTransform {
    val c: Context
    import c.universe._
    def myThis = c.prefix.tree
    def emtype = tq"_root_.gama.EnclosingModule"
  }

  class doExtract(val c: Context) extends CoreTransform {
    import c.universe._
    def onearg(position: c.Tree): c.Tree =
      q"$myThis.doExtract($position, implicitly[$emtype])"
    def twoarg(left_pos: c.Tree, right_pos: c.Tree): c.Tree =
      q"$myThis.doExtract($left_pos, $right_pos, implicitly[$emtype])"
  }
  
  class doLookup(val c: Context) extends CoreTransform {
    import c.universe._
    def onearg(selector: c.Tree): c.Tree =
      q"$myThis.doLookup($selector, implicitly[$emtype])"
  }
}

