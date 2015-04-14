package gama
package internal

trait ExtractableImpl {
  self: Element =>
  // TODO: Very similar to AccessibleImpl... somewhat to consolidate something?
  protected[gama] def doExtract(position: Int, em: EnclosingModule): Bool =
    makeExtract(Bool(), position, position, em)
  protected[gama] def doExtract(left_pos: Int, right_pos: Int, em: EnclosingModule): UInt =
    makeExtract(UInt(math.abs(left_pos-right_pos)), left_pos, right_pos, em)

  protected[gama] def makeExtract[E<:Element](retval: E, left_pos: Int, right_pos: Int, em: EnclosingModule): E = {
    val spell: NodeSpell[ExtractedNode] = node match {
      case _: Connectable   => (ConnectableExtractedSpell(em))
      case _: Synthesizable => (NonConnectableExtractedSpell(em))
      case e => (throw new ExpectedNodeException("Synthesizable","${e.getClass.getSimpleName}"))
    }
    Desc.generate(retval.rebind(spell))(rv => ExtractDesc(this, left_pos, right_pos, rv, em))
    // TODO: something special if this.node is also an ExtractedNode?
    // TODO: Disallow these updates for select types? ie. SubConnectable, Connectable, NonConnectable
  }
}

trait ExtractDescImpl {
  self: ExtractDesc =>
    def validateRetVal(): Unit = NodeCheck.assertExtractedNode(retVal)
    def genJournalEntry = Some(CreateExtract(this))
}

