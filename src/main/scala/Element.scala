package gama

abstract class Element[+NS<:NodeStore] extends Data {
  // Current list of mutable state and copy status:
  //  -> node (not copied)
  //  -> direction (lost on copies)

  protected[gama] def generateStorage: NS

  private[this] var node: Option[Node[NS]] = None
  final protected[gama] def isBound: Boolean = node.isDefined
  final def getNode: Node[NS] = node.getOrElse(throw new Exception("Attempted to getNode on unbound Element"))
  // !!!NOTE: Type Safety Workaround: NodeStore so that NS can be covariant
  final protected[gama] def bind(spell: Element[NodeStore]=>Node[NodeStore]) = {
    if(node == None) { node = Some(spell(this).asInstanceOf[Node[NS]]) }
    else {throw new Exception("Attempted to bind already-bound Element")}
    assert(getNode.storage == generateStorage || getNode.storage == generateStorage.default,
      "Element must be bound to Node using Element's generated NodeStore (or assoc. default)")
      // Sanity-check that the binding happened and of the right Node type
  }
  
  private[this] var direction: Option[IODirection] = None
  final protected[gama] def getDirection: Option[IODirection] = node.map(_ match {
      case port: Port[_] => (Some(port.direction))
      case _ => None // !!!CONSIDER: Throw exception?
    }).getOrElse(direction)
    // If attached to a Node, gives direction of Port it is describing; otherwise,
    //   getDirection is being used to construct a Port so give stored direction

  def asInput  = {direction = Some(INPUT); this}
  def asOutput = {direction = Some(OUTPUT); this}
  def flip = {
    direction = direction.map(_ match {
      case INPUT  => OUTPUT
      case OUTPUT => INPUT
    })
    // !!!CONSIDER: Throw exception if direction is None?
    this
  }
  // !!!CONSIDER: Throw exception if node is not None for these 3 (as they will do nothing)?


  // These just exist to handle code common to the concrete subclasses but necessary to fulfill type contracts
  protected[gama] def unsafeAssign(target: Data): this.type = {
    // TODO: Consider adding more checks....
    target match {
      case e: Element[NodeStore] => {
        assert(e.generateStorage.getClass == generateStorage.getClass,
          s"Invalid assign: NodeStore-level: ${this.getClass.getName} (with ${generateStorage.getClass.getName}) and ${e.getClass.getName} (with ${e.generateStorage.getClass.getName})}")
        }
        handleAssign(e.asInstanceOf[Element[NS]].getNode)
      case _ => throw new Exception(s"Invalid assign: Data-level: Element(${this.getClass.getName}) and ${target.getClass.getName}")
    }
    this
  }
  protected[gama] def unsafeMux(cond: Node[RawBits], tc: Data, fc: Data): Unit = {
    // TODO: Consider Adding more checks...
    // This check ensures fc and tc are subtypes of this
    assert(getClass.isAssignableFrom(tc.getClass) && getClass.isAssignableFrom(fc.getClass),
      s"Invalid Mux: ${this.getClass.getName} with ${tc.getClass.getName} (tc) and ${fc.getClass.getName} (fc)")
    // Pass check so get 'typesafe' tc and fc
    val ts_tc = tc.asInstanceOf[Element[NS]]
    val ts_fc = fc.asInstanceOf[Element[NS]]
    val opnode = new Mux[NS](generateStorage.default, cond, ts_tc.getNode, ts_fc.getNode)
    bind(_=>opnode)
  }

  protected[this] def handleAssign(target: Node[NS]): Unit = {
    getNode match {
      case aNode: Assignable[NS] => (aNode.forceAssign(target))
      case other => throw new Exception(s"Cannot assign to node of type ${other.getClass}")
    }
  }

}

abstract class Bits(initial_width: Option[Int]) extends Element[RawBits] {
  def generateStorage = RawBits(initial_width)
}

