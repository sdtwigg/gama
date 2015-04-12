package gama
import internal._

@annotation.implicitNotFound("""Cannot create Mem[${D}]. 
Most common reason is that no self-connect operation (ConnectSelf[${D}]) available""")
trait Storable[D<:Data] { def writer: ConnectSelf[D] }
// Only reason Storable trait exists is so that failing to create a Vec gives a specialized error message
object Storable {
  implicit def storer[D<:Data: ConnectSelf]: Storable[D] = new Storable[D] {val writer = implicitly[ConnectSelf[D]]}
} // TODO: REMOVE DEPEDENCY ON SELFMUXABLE?
// TODO: IS THIS EVEN REQUIRED AS CAN STRAIGHT COPY MODEL IN MEM?

object Mem {
  def apply[D<:Data: Storable](model: D, depth: Int)(implicit em: EnclosingModule): Mem[D] = {
    val newmemory = new Mem(model.copy.rebind(MemSpecSpell(em)), depth, em)
    em.getActiveJournal.append(CreateMem(newmemory))

    newmemory
  }
} // TODO: MEMORY WRITE MASKS?

final class Mem[D<:Data: Storable] private (model: D, val depth: Int, protected[gama] val em: EnclosingModule) extends MemAccessible[D] with Nameable {
  def collection = this
  protected[gama] def elemType: D = model // TODO: BIND TO A SEALED, NON-SYNTH NODE?
  
  protected[gama] def lookupIsConnectable(selector: UIntLike): Boolean = true
  // TODO: REMOVE WHEN ACCESSIBLE REFACTORED? probably no, Accessible works w Mem now
  // TODO: CONSIDER: Return false so writes can only be done through write call. Otherwise can do myMemOfBundle(myAddr).myBundleField := myUpdate, which is strange.
  // However, that may be a good story for masked writes???

  def propogateName(newname: NameTree, newsource: NameSource): Unit = {} 

  // external API
  def write(addr: UIntLike, source: D)(implicit acc_em: EnclosingModule): Unit = {
    if(acc_em != em) { throw CrossedMemoryAccessException(acc_em, em) }
    val accessor = makeAccessor(addr, em)
    implicitly[Storable[D]].writer.connectSelf(Sink(accessor), Source(source), em)
  }
}
