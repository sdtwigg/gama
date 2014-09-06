package gama

object Bundle {
  val reflect_blacklist = scala.collection.immutable.HashSet(
    "copy", "asInput", "asOutput", "flip"
  )
}

class Bundle extends Aggregate {
  private lazy val subdata: Map[String,Data] = {
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

    unique_candidates.toMap
  }

  def getSubdata = subdata.toVector.sortBy(_._1).map(_._2) // Map may lose sorting so sort again
  def copy: this.type = {
    // Use some type reflection to try and auto-create copy-method
    //   for user-created (possibly anonymous) bundles
    val constructor = getClass.getConstructors.head
    require(constructor.getParameterTypes.size==0, s"Bundle ${getClass} has primary constructor that requires parameters. Thus, you must supply an overriden copy function")
    constructor.newInstance().asInstanceOf[this.type]
  }
  
  def test = subdata
  
  protected[gama] def unsafeAssign(target: Data) = {
    // TODO: better checks?
    target match {
      case b: Bundle => {
        subdata.foreach({ case (fd_name, fd_ptr) =>
          fd_ptr.unsafeAssign(b.subdata.getOrElse(fd_name, ???)) // TODO: ADD EXCEPTION
        })
      }
      case _ => throw new Exception(s"Invalid assign: Data-level: Bundle(${this.getClass.getName}) and ${target.getClass.getName}")
    }
    this
  }
  protected[gama] def unsafeMux(cond: Node[RawBits], tc: Data, fc: Data): Unit = {
    // TODO: better checks?
    val ts_tc = tc match {
      case b: Bundle => (b)
      case _ => throw new Exception(s"Invalid Mux: ${this.getClass.getName} with ${tc.getClass.getName} (tc) and ${fc.getClass.getName} (fc)")
    }
    val ts_fc = fc match {
      case b: Bundle => (b)
      case _ => throw new Exception(s"Invalid Mux: ${this.getClass.getName} with ${tc.getClass.getName} (tc) and ${fc.getClass.getName} (fc)")
    }

    subdata.foreach({ case (fd_name, fd_ptr) => {
      val fd_tc_ptr = ts_tc.subdata.getOrElse(fd_name, ???) // TODO: exception text
      val fd_fc_ptr = ts_fc.subdata.getOrElse(fd_name, ???) // TODO: exception text
      fd_ptr.unsafeMux(cond, fd_tc_ptr, fd_fc_ptr)
    } })

  }
}

class MyBundle(w: Int) extends Bundle {
  val in  = UInt(INPUT, w)
  val out = UInt(OUTPUT, w)

  val a = 1
  def b = a

  def other_in = in

  override def copy: this.type = (new MyBundle(w)).asInstanceOf[this.type]
}
