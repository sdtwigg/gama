package gama
package frontend
package implementation
package macrodefs

protected[gama] object AnnoImpl {
  import scala.reflect.macros.whitebox.Context

  def moduleimpl(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val allowInnerClasses: Boolean = c.inferImplicitValue(c.typeOf[gama.frontend.unsafe.internal.unsafeFlags.MIC], silent=true) != EmptyTree
    case class innerclasschecker(parentName: String) extends Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => {
          c.warning(c.enclosingPosition, s"Module-type $parentName has inner class named ${tpname.toString}.")
        }
        case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => {
          c.warning(c.enclosingPosition, s"Module-type $parentName has inner trait named ${tpname.toString}.")
        }
        case _ => super.traverse(tree)
      }
    }
    case object valnamer extends Transformer {
      override def transform(tree: Tree) = tree match {
        case q"$mods val $tname: $ttree = $assign" if mods == (Modifiers()) => {
          // Check Modifiers so Only match for val x = ...
          //   as ValDef also used in Class, Function, etc. definitions
          val TermName(name) = tname
          val suggestion = q"$name"
          val subtree = transform(assign)
          q"$mods val $tname: $ttree = _root_.gama.frontend.implementation.InternalName($subtree, $suggestion, _root_.gama.frontend.implementation.NameFromMacro)"
        }
        case _ => super.transform(tree)
      }
    }

    val xformd: Seq[Tree] = annottees.map(tree => tree match {
      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => {
        if(!allowInnerClasses) {
          val parentName = tpname.toString
          innerclasschecker(parentName).traverseTrees(earlydefns)
          innerclasschecker(parentName).traverseTrees(stats)
        }

        val newBody = valnamer.transformTrees(stats)
        q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$newBody }"
      }
      case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => {
        if(!allowInnerClasses) {
          val parentName = tpname.toString
          innerclasschecker(parentName).traverseTrees(earlydefns)
          innerclasschecker(parentName).traverseTrees(stats)
        }

        val newBody = valnamer.transformTrees(stats)
        q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$newBody }"
      }
      case _ => tree
    })
    
    q"..$xformd"
  }
  
  def bundleimpl(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    import Flag._

    def getTermName(in: ValDef): TermName = in match {case q"$mods val $tname: $tpt = $expr" => tname}
    def getNoArgDefs(in: Seq[Tree]): Seq[String] = { //clever trick from delegate example
      in.flatMap(_ match{
        case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" if paramss.isEmpty => Some(tname.toString)
        case _ => None
      })
    }

    val xformd: Seq[Tree] = annottees.map(_ match {
      case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => {
        c.abort(c.enclosingPosition, "@bundle annotation can only be used on concrete classes")
      }
      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => {
        if(mods.hasFlag(ABSTRACT)) { c.abort(c.enclosingPosition, "@bundle annotation can only be used on concrete classes") }
        val termparamss = paramss.map(a=>a.map(b=>getTermName(b)))
        val mysimplecopy = q"override def simplecopy: this.type = (new $tpname(...$termparamss)).asInstanceOf[this.type]"
        val newbody = stats ++ Seq(
          if(getNoArgDefs(stats).contains("simplecopy")) q""
          else mysimplecopy 
        )
        q"""
          $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents {
            $self =>
              ..$newbody
          }
        """
      }
      case other => other
    })

    q"..$xformd"
  }
  
  def probeimpl(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    import Flag._
    
    annottees.foreach(tree => println(showCode(tree)))
    q"..$annottees"
  }
}
