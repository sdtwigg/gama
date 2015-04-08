package gama
package internal

sealed abstract class NameSource(private val priority: Int) extends Ordered[NameSource] {
  def compare(that: NameSource) = priority.compare(that.priority)
}
case object NameFromMath  extends NameSource(0) // e.g. when ops folded, like 1 + 1
case object NameFromTemp  extends NameSource(1) // e.g. T0, R1, A2, M3, W4

case object NameFromMacro extends NameSource(2)
case object NameFromUser  extends NameSource(3)

case object NameFromIO    extends NameSource(10) // Name from an IO setup of some variety
case object NameFromLit   extends NameSource(11) // Name is a NameLit

trait Nameable { // MUTABLE STATE: name
  protected[gama] def propogateName(newname: NameTree, newsource: NameSource): Unit
  
  private[this] var nameDetails: Option[Tuple2[NameTree, NameSource]] = None
  protected[gama] def name: Option[NameTree] = nameDetails.map(_._1)
  protected[gama] def nameSource: Option[NameSource] = nameDetails.map(_._2)

  protected[gama] def forceSetName(newname: NameTree, newsource: NameSource, propogate: Boolean): Unit = {
    nameDetails = Some(Tuple2(newname, newsource))
    if(propogate) { propogateName(newname, newsource) }
  }
  protected[gama] def checkedSetName(newname: NameTree, newsource: NameSource, propogate: Boolean): Unit = {
    if(nameDetails match {
      case None => true
      case Some((_, oldsource)) if newsource > oldsource => true
      case _ => false
    }) {
      forceSetName(newname, newsource, propogate)
    }
  }
}

object InternalName {
  def apply[A](in: A, suggestion: String, priority: NameSource): A = {
    in match {
      case x: Data => (x.checkedSetName(NameTerm(suggestion), priority, true)) 
      case _ => 
    }
    in
  }
}

