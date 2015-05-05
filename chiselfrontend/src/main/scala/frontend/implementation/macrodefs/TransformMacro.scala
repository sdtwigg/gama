package gama
package frontend
package implementation
package macrodefs

protected[gama] object TransformMacro {
  import scala.reflect.macros.blackbox.Context

  // Amusingly, could simplify this file with macros;
  // However, may make frontend too complex

  sealed trait CoreTransform {
    val c: Context
    import c.universe._
    def myThis = c.prefix.tree
    def constructInfo = {
      val emtype = tq"_root_.gama.frontend.EnclosingModule"
      val sfile = c.enclosingPosition.source.file.name.toString
      val sline = c.enclosingPosition.line.toString
      q"_root_.gama.frontend.EnclosureInfo(implicitly[$emtype], _root_.scala.Some(_root_.gama.UserspaceInfo($sfile,$sline)))"
    }
  }

  abstract class UnaryOpTransform(opterm: String) extends CoreTransform {
    import c.universe._
    def noparen: c.Tree = {
      val targetf = TermName(opterm)
      q"$myThis.$targetf($constructInfo)"
    }
    def paren(): c.Tree = {
      val targetf = TermName(opterm)
      q"$myThis.$targetf($constructInfo)"
    }
  }
  abstract class BinaryOpTransform(opterm: String) extends CoreTransform {
    import c.universe._
    def thatarg(that: c.Tree): c.Tree = {
      val targetf = TermName(opterm)
      q"$myThis.$targetf($that, $constructInfo)"
    }
  }

  class doExtract(val c: Context) extends CoreTransform {
    import c.universe._
    def onearg(position: c.Tree): c.Tree =
      q"$myThis.doExtract($position, $constructInfo)"
    def twoarg(left_pos: c.Tree, right_pos: c.Tree): c.Tree =
      q"$myThis.doExtract($left_pos, $right_pos, $constructInfo)"
  }
  
  class doLookup(val c: Context) extends CoreTransform {
    import c.universe._
    def onearg(selector: c.Tree): c.Tree =
      q"$myThis.doLookup($selector, $constructInfo)"
  }
  
  class doConnectTo(val c: Context) extends CoreTransform {
    import c.universe._
    def sourcearg(source: c.Tree): c.Tree =
      q"$myThis.doConnectTo($source, $constructInfo)"
  }
  
  class doBiConnect(val c: Context) extends CoreTransform {
    import c.universe._
    def rightarg(right: c.Tree): c.Tree =
      q"$myThis.doBiConnect($right, $constructInfo)"
  }
  
  class doMemWrite(val c: Context) extends CoreTransform {
    import c.universe._
    def twoarg(addr: c.Tree, source: c.Tree): c.Tree =
      q"$myThis.doMemWrite($addr, $source, $constructInfo)"
  }
  
  class doConstant(val c: Context) extends CoreTransform {
    import c.universe._
    def xform[D<:Data: c.WeakTypeTag](in: c.Tree): c.Tree = {
      val TP = c.weakTypeOf[D]
      q"$myThis.doConstant[$TP]($in, $constructInfo)"
    }
  }
  class doNodeXFORM(val c: Context) extends CoreTransform {
    import c.universe._
    def xform[D<:Data: c.WeakTypeTag](model: c.Tree): c.Tree = {
      val TP = c.weakTypeOf[D]
      val storerType = tq"_root_.gama.frontend.Storable[$TP]"
      q"$myThis.doNodeXFORM[$TP]($model, implicitly[$storerType], $constructInfo)"
    }
    def xinit[D<:Data: c.WeakTypeTag](init: c.Tree): c.Tree = {
      val TP = c.weakTypeOf[D]
      val storerType = tq"_root_.gama.frontend.Storable[$TP]"
      q"$myThis.doNodeXFORM[$TP]($init, implicitly[$storerType], $constructInfo)"
    }
    def xmodel_init[D<:Data: c.WeakTypeTag](model: c.Tree, init: c.Tree): c.Tree = {
      val TP = c.weakTypeOf[D]
      val storerType = tq"_root_.gama.frontend.Storable[$TP]"
      q"$myThis.doNodeXFORM[$TP]($model, $init, implicitly[$storerType], $constructInfo)"
    }
  }
  class doMux(val c: Context) extends CoreTransform {
    import c.universe._
    def xform[RT<:Data: c.WeakTypeTag](cond: c.Tree, tc: c.Tree, fc: c.Tree): c.Tree = {
      val TP = c.weakTypeOf[RT]
      val muxerType = tq"_root_.gama.frontend.Muxable[$TP]"
      q"$myThis.doMux[$TP]($cond, $tc, $fc, implicitly[$muxerType], $constructInfo)"
    }
  }
  class doMem(val c: Context) extends CoreTransform {
    import c.universe._
    def xform[RT<:Data: c.WeakTypeTag](model: c.Tree, depth: c.Tree): c.Tree = {
      val TP = c.weakTypeOf[RT]
      val storerType = tq"_root_.gama.frontend.Storable[$TP]"
      q"$myThis.doMem[$TP]($model, $depth, implicitly[$storerType], $constructInfo)"
    }
  }
  
  // Unary transforms
  class do_andR(val c: Context) extends  UnaryOpTransform("do_andR")
  class do_orR (val c: Context) extends  UnaryOpTransform("do_orR")
  class do_xorR(val c: Context) extends  UnaryOpTransform("do_xorR")
  
  class do_not(val c: Context) extends  UnaryOpTransform("do_not")
  class do_neg(val c: Context) extends  UnaryOpTransform("do_neg")
  
  class do_toUInt(val c: Context) extends  UnaryOpTransform("do_toUInt")
  class do_toSInt(val c: Context) extends  UnaryOpTransform("do_toSInt")
  class do_asUInt(val c: Context) extends  UnaryOpTransform("do_asUInt")
  class do_asSInt(val c: Context) extends  UnaryOpTransform("do_asSInt")

  // Binary transforms
  class do_cat  (val c: Context)  extends BinaryOpTransform("do_cat")
  class do_lshft(val c: Context)  extends BinaryOpTransform("do_lshft")
  class do_rshft(val c: Context)  extends BinaryOpTransform("do_rshft")
  
  class do_add(val c: Context)  extends BinaryOpTransform("do_add")
  class do_sub(val c: Context)  extends BinaryOpTransform("do_sub")
  class do_mul(val c: Context)  extends BinaryOpTransform("do_mul")
  class do_div(val c: Context)  extends BinaryOpTransform("do_div")
  class do_mod(val c: Context)  extends BinaryOpTransform("do_mod")
  
  class do_eq (val c: Context)  extends BinaryOpTransform("do_eq")
  class do_neq(val c: Context)  extends BinaryOpTransform("do_neq")
  
  class do_lt (val c: Context)  extends BinaryOpTransform("do_lt")
  class do_lte(val c: Context)  extends BinaryOpTransform("do_lte")
  class do_gt (val c: Context)  extends BinaryOpTransform("do_gt")
  class do_gte(val c: Context)  extends BinaryOpTransform("do_gte")
  
  class do_and(val c: Context)  extends BinaryOpTransform("do_and")
  class do_or (val c: Context)  extends BinaryOpTransform("do_or")
  class do_xor(val c: Context)  extends BinaryOpTransform("do_xor")
  
  // Boolean extensions
  class do_andB(val c: Context)  extends BinaryOpTransform("do_andB")
  class do_orB (val c: Context)  extends BinaryOpTransform("do_orB")
  class do_xorB(val c: Context)  extends BinaryOpTransform("do_xorB")
}

