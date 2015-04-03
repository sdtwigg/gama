package gama
package internal

sealed abstract class NamePriority(private val priority: Int) extends Ordered[NamePriority] {
  def compare(that: NamePriority) = priority.compare(that.priority)
}
case object NameFromAggP  extends NamePriority(0) // Name from Aggregate Parent
case object NameFromTemp  extends NamePriority(1)
case object NameFromMacro extends NamePriority(2)
case object NameFromUser  extends NamePriority(3)
case object NameFromIO    extends NamePriority(4) // Name from an IO call propogating down now

trait Nameable { // MUTABLE STATE: name
  private[this] var nameDetails: Option[Tuple2[String, NamePriority]] = None
  def name: Option[String] = nameDetails.map(_._1)
  protected[gama] def applyName(suggestion: String, priority: NamePriority): Unit
  protected[gama] def name_=(in: Tuple2[String, NamePriority]): Option[String] = {
    val (suggestion, priority) = in
    nameDetails match {
      case None => {nameDetails = Option((suggestion,priority))}
      case Some((oldName, oldPriority)) if priority > oldPriority =>
        {nameDetails = Option((suggestion, priority))}
      case _ => 
    }
    name
  }
}

object InternalName {
  def apply[A](in: A, suggestion: String, priority: NamePriority): A = {
    in match {
      case x: Data => (x.applyName(suggestion,priority))
      case _ => 
    }
    in
  }
}

