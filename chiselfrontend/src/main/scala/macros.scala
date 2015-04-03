package gama
package macros

import scala.reflect.macros.whitebox.Context //consider blackbox
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class module extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro macroAnno.moduleimpl
}

protected[gama] object macroAnno {
  def moduleimpl(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    
    object vdoer extends Transformer {
      override def transform(tree: Tree) = tree match {
        case q"$mods val $tname: $ttree = $assign" if mods == (Modifiers()) => {
          // Check Modifiers so Only match for val x = ...
          //   as ValDef also used in Class, Function, etc. definitions
          val TermName(name) = tname
          val suggestion = q"$name"
          val subtree = transform(assign)
          q"$mods val $tname: $ttree = _root_.gama.internal.InternalName($subtree, $suggestion, _root_.gama.internal.NameFromMacro)"
        }
        case _ => super.transform(tree)
      }
    }

    val named = annottees.map(vdoer.transform(_))
    named.foreach(tree => println(showCode(tree)))
    q"..$named"
  }
}
