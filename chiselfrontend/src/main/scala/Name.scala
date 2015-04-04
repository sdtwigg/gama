package gama
package internal

sealed abstract class NamePriority(private val priority: Int) extends Ordered[NamePriority] {
  def compare(that: NamePriority) = priority.compare(that.priority)
}
case object NameFromMath  extends NamePriority(0)
case object NameFromTemp  extends NamePriority(1)
case object NameFromMacro extends NamePriority(2)
case object NameFromUser  extends NamePriority(3)
case object NameFromIO    extends NamePriority(4) // Name from an IO call propogating down now

trait Nameable { // MUTABLE STATE: name
  private[this] var nameDetails: Option[Tuple2[String, NamePriority]] = None
  def name: Option[String] = nameDetails.map(_._1)
  protected[gama] def namePriority: Option[NamePriority] = nameDetails.map(_._2)
  protected[gama] def propogateName(): Unit
  protected[gama] def name_=(in: Tuple2[String, NamePriority]): Unit = {
    val (suggestion, priority) = in
    if(nameDetails match {
      case None => true
      case Some((oldName, oldPriority)) if priority > oldPriority => true
      case _ => false
    }) {
      nameDetails = Some(Tuple2(suggestion, priority))
      propogateName()
    }
    name
  }
}

object InternalName {
  def apply[A](in: A, suggestion: String, priority: NamePriority): A = {
    in match {
      case x: Data => (x.name = (suggestion,priority))
      case _ => 
    }
    in
  }
}

