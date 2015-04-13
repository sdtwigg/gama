package gama
import internal._

@annotation.implicitNotFound("""Cannot create Storable[${D}] for Mem[${D}]. 
Most common reason is that no self-connect operation (ConnectTo[${D},${D}]) available and thus the memory would not be writable""")
trait Storable[D<:Data] { def writer: ConnectTo[D,D] }
// Only reason Storable trait exists is so that failing to create a Vec gives a specialized error message
object Storable {
  implicit def storer[D<:Data](implicit ev: ConnectTo[D,D]): Storable[D] = new Storable[D] {val writer = implicitly[ConnectTo[D,D]]}
}

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
  def write[From<:Data](addr: UIntLike, source: From)(implicit acc_em: EnclosingModule, writer: ConnectTo[D, From]): Unit = {
    if(acc_em != em) { throw CrossedMemoryAccessException(acc_em, em) }
    val accessor = makeAccessor(addr, em)
    writer.connect(Sink(accessor), Source(source), em)
  }
}
