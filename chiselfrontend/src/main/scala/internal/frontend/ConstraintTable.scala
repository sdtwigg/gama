package gama
package internal
package frontend

import scala.collection.mutable.{HashMap=>HMap, HashSet=>HSet}

sealed trait WidthConstraint
case class WidthLit(lit: Int) extends WidthConstraint
case class WidthRef(ref: TypeTrace) extends WidthConstraint
case class WidthAdd(terms: Vector[WidthConstraint]) extends WidthConstraint // l + r
case class WidthMax(consts: Set[WidthConstraint]) extends WidthConstraint // max(1, 2, 3, ...)
case class WidthBitM(const: WidthConstraint) extends WidthConstraint // 2^const-1
case class FORCEWidthLit(forced: Int) extends WidthConstraint
  // override value that forces width to be this, TODO: is this OK?

class ConstraintTable {
  val table = HMap.empty[TypeTrace, Set[WidthConstraint]]

  def add(unknown: TypeTrace, constraint: WidthConstraint): Unit = {
    table.update(unknown, (table.getOrElse(unknown, Set.empty[WidthConstraint]) + constraint))
  }
  def get(unknown: TypeTrace): Option[WidthConstraint] = table.get(unknown).map(WidthMax(_))

  def solve: TypeTrace=>Option[Int] = {
    // First, some helper functions
    def replace(target: WidthConstraint)(implicit lookup: TypeTrace=>Option[WidthConstraint]): WidthConstraint = 
      target match {
        case WidthRef(ref) => lookup(ref).getOrElse(target)

        case WidthAdd(terms)  => WidthAdd(terms.map(replace(_)))
        case WidthMax(consts) => WidthMax(consts.map(replace(_)))
        case WidthBitM(const) => WidthBitM(replace(const))
        
        case WidthLit(_) => target
        case FORCEWidthLit(lit) => throw new Exception("Internal Error: Width Force Failure")
          // These should be gone by now
    }
    def simplify(target: WidthConstraint): WidthConstraint = target match {
      case WidthLit(_) | WidthRef(_) | FORCEWidthLit(_) => target // these are unsimplifiable
      case WidthMax(consts) => simplifyMax(consts)
      case WidthAdd(terms)  => simplifyAdd(terms)
      case WidthBitM(term)  => simplify(term) match {
                                 case WidthLit(lit) => WidthLit((2 << (lit-1))-1)
                                 case other => other
                               }
    }
    def simplifyMax(consts: Set[WidthConstraint]): WidthConstraint = {
      val forceLit: Option[Int] = consts.flatMap({
        case FORCEWidthLit(lit) => Some(lit)
        case _ => None
      }).headOption
      if(!forceLit.isEmpty) WidthLit(forceLit.get)
      else {
        val sconsts: Set[WidthConstraint] = consts.map(simplify(_)).flatMap({
          case WidthMax(cs) => cs
          case other => Some(other)
        })
        val (lits, others) = sconsts.partition(_.isInstanceOf[WidthLit])
        val reduced = if(!lits.isEmpty) {
          val newmax = lits.map(x=>(x: @unchecked) match {case WidthLit(lit) => lit}).reduce(math.max(_,_))
          others + WidthLit(newmax)
        } else others
        if(reduced.size==1) reduced.head else WidthMax(reduced)
      }
    }
    def simplifyAdd(consts: Vector[WidthConstraint]): WidthConstraint = {
      val sconsts: Vector[WidthConstraint] = consts.map(simplify(_)).flatMap({
        case WidthAdd(cs) => cs
        case other => Some(other)
      })
      val (lits, others) = sconsts.partition(_.isInstanceOf[WidthLit])
      val reduced = if(!lits.isEmpty) {
        val newlit = lits.map(x=>(x: @unchecked) match {case WidthLit(lit) => lit}).reduce(_+_)
        others :+ WidthLit(newlit)
      } else others
      if(reduced.size==1) reduced.head else WidthAdd(reduced)
    }

    
    def doPass(order: Seq[TypeTrace], prelookup: TypeTrace=>Option[WidthConstraint]): TypeTrace=>Option[WidthConstraint] = {
      val result = HMap.empty[TypeTrace, WidthConstraint]
      val lookup: TypeTrace=>Option[WidthConstraint] = (tt) => result.get(tt)
      for(unknown <- order; constraint <- prelookup(unknown)){
        val newconstraint = simplify( replace(simplify(constraint))(lookup) )
        result.update(unknown, newconstraint)
      }

      lookup
    }
    // FIRST PASS
    val first_pass_order   = table.keys.toSeq
    val first_pass_lookup  = doPass(first_pass_order, (tt) => get(tt))
    // SECOND PASS (in reverse)
    val second_pass_order  = first_pass_order.reverse
    val second_pass_lookup = doPass(second_pass_order, first_pass_lookup)

    val final_result: TypeTrace=>Option[Int] = (tt) => second_pass_lookup(tt) match {
      case Some(WidthLit(answer)) => Some(answer)
      case _ => None // Give different message when uninferrable vs just lost data?
    }

    // DEBUGGING
    val dprint = IRReader.Colorful
    def parseConstraint(c: WidthConstraint): String = c match {
      case WidthLit(lit)      => s"$lit"
      case WidthRef(ref)      => s"${dprint.parseTT(ref)}"
      case WidthAdd(terms)    =>  terms.map(parseConstraint(_)) mkString("(","+",")")
      case WidthMax(consts)   => consts.map(parseConstraint(_)) mkString("max(",", ", ")")
      case WidthBitM(const)   => s"(2^${parseConstraint(const)}-1)"
      case FORCEWidthLit(lit) => s"== FORCED: $lit"
    }
    println("FIRST PASS")
    for( unknown <- first_pass_order; in <- get(unknown); out <- first_pass_lookup(unknown) ) {
      println(s"${dprint.parseTT(unknown)} >= ${parseConstraint(in)} ---> >= ${parseConstraint(out)}")
    }
    println("")
    println("SECOND PASS")
    for( unknown <- second_pass_order; in <- first_pass_lookup(unknown); out <- second_pass_lookup(unknown) ) {
      println(s"${dprint.parseTT(unknown)} >= ${parseConstraint(in)} ---> >= ${parseConstraint(out)}")
    }


    final_result
  }
}
