package gama
import internal._

case class NoDataFoundException(containerType: String)
  extends ChiselException(s"Could not find any Data vals in ${containerType}")
abstract class Bundle extends HardwareTuple {
  protected[gama] lazy val subfields: Seq[Tuple2[String, Data]] = {
    import scala.reflect.runtime.{universe=>ru}
    import Reflection._

    val myMirror = getInstanceMirror(this)
    val myType = myMirror.symbol.toType

    val allTerms: Seq[ru.TermSymbol] = myType.members.toList filter(_.isTerm) map(_.asTerm)
    val termCandidates = allTerms.filter(ts =>
      ( ts.isVal || (ts.isGetter && ts.isStable) )
    )
    def findGetterCandidate(start: ru.TermSymbol): Option[ru.TermSymbol] = {
      allTerms.find(cand => (cand.name == start.name) && cand.isPublic && cand.isMethod)
    }
    val getterCandidates = termCandidates.flatMap(ts => findGetterCandidate(ts)).distinct
    val dataGetters = getterCandidates.map(_.asMethod).filter(_.returnType <:< ru.typeOf[Data])
    
    val result = dataGetters.map(dataGetter =>
      (dataGetter.name.toString, myMirror.reflectMethod(dataGetter).apply().asInstanceOf[Data])
    ).sortBy(_._1)
    if(result.isEmpty) {throw NoDataFoundException(this.getClass.getName)}
    result
  }
//  def copy: this.type
}

case class NeedCopyMethodException(containerType: String)
  extends ChiselException(s"AnonBundle ${containerType} needs an explicit copy method")
trait Anon extends Bundle {
  def copy: this.type = throw NeedCopyMethodException(this.getClass.getName)
}
