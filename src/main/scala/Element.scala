package gama

abstract class Element[+NS<:NodeStore] extends Data {
  // Current list of mutable state and copy status:
  //  -> node (not copied)
  //  -> direction (copy via copyDirection)

  protected[gama] def generateStorage: NS


  protected[gama] def flatten = Vector(this)

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
  
  private var direction: Option[IODirection] = None
  protected def copyDirection(target: Element[_]): Unit = {direction = target.direction}
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

  protected[this] def handleAssign(target: Node[NS]) = {
    getNode match {
      case aNode: Assignable[NS] => (aNode.forceAssign(target))
      case other => throw new Exception(s"Cannot assign to node of type ${other.getClass}")
    }
  }
}

abstract class Bits(initial_width: Option[Int]) extends Element[RawBits] {
  def generateStorage = RawBits(initial_width)
}

