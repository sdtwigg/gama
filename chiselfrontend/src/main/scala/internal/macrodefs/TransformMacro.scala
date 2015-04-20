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

  abstract class UnaryOpTransform(opterm: String) extends CoreTransform {
    import c.universe._
    def noparen: c.Tree = {
      val targetf = TermName(opterm)
      q"$myThis.$targetf(implicitly[$emtype])"
    }
    def paren(): c.Tree = {
      val targetf = TermName(opterm)
      q"$myThis.$targetf(implicitly[$emtype])"
    }
  }
  abstract class BinaryOpTransform(opterm: String) extends CoreTransform {
    import c.universe._
    def thatarg(that: c.Tree): c.Tree = {
      val targetf = TermName(opterm)
      q"$myThis.$targetf($that, implicitly[$emtype])"
    }
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

  class do_eq (val c: Context)  extends BinaryOpTransform("do_eq")
  class do_neq(val c: Context)  extends BinaryOpTransform("do_neq")
  class do_cat(val c: Context)  extends BinaryOpTransform("do_cat")
  
  class do_not(val c: Context) extends  UnaryOpTransform("do_not")

  class do_andR(val c: Context) extends  UnaryOpTransform("do_andR")
  class do_orR (val c: Context) extends  UnaryOpTransform("do_orR")
  class do_xorR(val c: Context) extends  UnaryOpTransform("do_xorR")
}

