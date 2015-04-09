package gama
package internal

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
case object NameFromMath  extends NameTentative(0) // e.g. when ops folded, like 1 + 1
case object NameFromTemp  extends NameTentative(1) // e.g. T0, R1, A2, M3, W4

case object NameFromMacro extends NameTentative(2)
case object NameFromUser  extends NameTentative(3)

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
      case x: Data => (x.checkedSetName(NameTerm(suggestion), priority, true)) 
      case _ => 
    }
    in
  }
}

