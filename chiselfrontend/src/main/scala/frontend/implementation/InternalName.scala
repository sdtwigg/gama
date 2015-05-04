package gama
package frontend
package implementation

sealed abstract class NameSource {
  protected def priority: Option[Int] // None indicates highest priority
  // THIS IS A PARTIAL ORDERING
  protected[gama] def < (that: NameSource) = (this.priority, that.priority) match {
    case (None, _)                 => false
    case (Some(left), Some(right)) => left < right
    case (Some(_),    None)        => true
  }
  protected[gama] def > (that: NameSource) = that < this
}
abstract class NameTentative(priority: Int) extends NameSource {def priority = Some(priority)}
case object NameFromInit  extends NameTentative(0) // Desc sets to NameUNKNOWN with this priority
case object NameFromMath  extends NameTentative(1) // e.g. when ops folded, like 1 + 1
case object NameFromTemp  extends NameTentative(2) // e.g. T0, R1, A2, M3, W4

case object NameFromMacro extends NameTentative(3)
case object NameFromUser  extends NameTentative(4)

// These names can only be set using forceSetName
abstract class NameForceOnly extends NameSource {def priority = None}
case object NameFromIO    extends NameForceOnly // Name from an IO setup of some variety
case object NameFromLit   extends NameForceOnly // Name is a NameLit

trait Nameable { // MUTABLE STATE: name
  protected[gama] def propogateName(newname: NameTree, newsource: NameSource): Unit
  
  private[this] var nameDetails: Option[Tuple2[NameTree, NameSource]] = None
  protected[gama] def name: Option[NameTree] = nameDetails.map(_._1)
  protected[gama] def nameSource: Option[NameSource] = nameDetails.map(_._2)

  protected[gama] def forceSetName(newname: NameTree, newsource: NameSource, propogate: Boolean): Unit = {
    nameDetails = Some(Tuple2(newname, newsource))
    if(propogate) { propogateName(newname, newsource) }
  }
  protected[gama] def checkedSetName(newname: NameTree, newsource: NameTentative, propogate: Boolean): Unit = {
    if(nameDetails match {
      case None => true
      case Some((_, oldsource)) if oldsource < newsource => true
      case _ => false
    }) {
      forceSetName(newname, newsource, propogate)
    }
  }
}

object InternalName {
  def apply[A](in: A, suggestion: String, priority: NameTentative): A = {
    in match {
      case x: Nameable => (x.checkedSetName(NameTerm(suggestion), priority, true)) 
      case _ => 
    }
    in
  }
}

// TODO: More intelligent naming of internal nodes
// Names should only start being propogates from things that are the root of a Desc
// Also, things that already have a name COULD be granted a ref or const.
//   Then AsHWConstant is unneeded
