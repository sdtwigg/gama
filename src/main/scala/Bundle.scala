package gama

object Bundle {
  val reflect_blacklist = scala.collection.immutable.HashSet(
    "copy", "asInput", "asOutput", "flip"
  )
}

class Bundle extends Aggregate {
  private[this] lazy val subdata: IndexedSeq[Tuple2[String,Data]] = {
    import java.lang.reflect.Modifier._
    // isAssignableFrom is from the java reflection API
    // classOf is a scala reflection utility

    val myclass = getClass()
    val allmethods = myclass.getMethods.toVector

    val filtered_methods = allmethods.filter(f =>   // Filter to methods that
      classOf[Data].isAssignableFrom(f.getReturnType) && // return a subtype of Data
      (f.getParameterTypes().size == 0) &&               // take no arguments
      !isStatic(f.getModifiers) &&                       // are not static methods (rare issue)
      !Bundle.reflect_blacklist.contains(f.getName)      // not on the blacklist
    )

    // transform into (name: String, target: Data) tuples
    val candidates = filtered_methods.map(f => (f.getName, f.invoke(this).asInstanceOf[Data]))
      // the asInstanceOf should NEVER fail due to filtering above
    
    // sort by name: important for what gets 'taken' (see below) and io emission order
    val sorted_candidates = candidates.sortBy(_._1)

    // filter out copies (take first seen, recall: previously sorted alphabetically by name)
    val seen_data = new scala.collection.mutable.HashSet[Data]
    val unique_candidates = sorted_candidates.map(cd =>
      if(seen_data.contains(cd._2)) None else {seen_data += cd._2; Some(cd)}).filter(_.isDefined).map(_.get)

    unique_candidates
  }

  def getSubdata = subdata.map(_._2)
  def copy: this.type = {
    // Use some type reflection to try and auto-create copy-method
    //   for user-created (possibly anonymous) bundles
    val constructor = getClass.getConstructors.head
    require(constructor.getParameterTypes.size==0, s"Bundle ${getClass} has primary constructor that requires parameters. Thus, you must supply an overriden copy function")
    constructor.newInstance().asInstanceOf[this.type]
  }
  
  def test = subdata
  
  protected[gama] def unsafeAssign(target: Data) = ???
  protected[gama] def unsafeMux(cond: Node[RawBits], tc: Data, fc: Data): Unit = ???
}

class MyBundle(w: Int) extends Bundle {
  val in  = UInt(INPUT, w)
  val out = UInt(OUTPUT, w)

  val a = 1
  def b = a

  def other_in = in

  override def copy: this.type = (new MyBundle(w)).asInstanceOf[this.type]
}
