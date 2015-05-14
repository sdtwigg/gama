package gama
package frontend
package implementation

trait ExtractableImpl {
  self: Element =>
  // TODO: Very similar to AccessibleImpl... somewhat to consolidate something?
  // external -> internal API
  def doExtract(position: Int, info: EnclosureInfo): Bool =
    makeExtract(Bool.build(node.resolveDirection), position, position, info)
  def doExtract(left_pos: Int, right_pos: Int, info: EnclosureInfo): UInt = {
    val length = math.abs(left_pos-right_pos)+1
    makeExtract(UInt.build(Some(length), node.resolveDirection), left_pos, right_pos, info)
  }

  protected[gama] def makeExtract[E<:Element](retval: E, left_pos: Int, right_pos: Int, info: EnclosureInfo): E = {
    val spell: NodeSpell[ExtractedNode] = node match {
      case _: ConnectableNode => (ConnectableExtractedSpell(info.em))
      case _: Synthesizable   => (NonConnectableExtractedSpell(info.em))
      case e => (throw new ExpectedNodeException("Synthesizable","${e.getClass.getSimpleName}"))
    }
    Desc.generate(retval.rebind(spell))(rv => ExtractDesc(this, left_pos, right_pos, rv, info))
    // TODO: something special if this.node is also an ExtractedNode?
    // TODO: Disallow these updates for select types? ie. SubConnectable, Connectable, NonConnectable
  }
}

trait ExtractDescImpl {
  self: ExtractDesc =>
    def validateRetVal(): Unit = NodeCheck.assertExtractedNode(retVal)
    def genJournalEntry = Some(journal.CreateExtract(this))
}

