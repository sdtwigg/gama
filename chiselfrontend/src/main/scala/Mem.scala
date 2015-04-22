package gama
import internal._

@annotation.implicitNotFound("""Cannot create Storable[${D}] for Mem[${D}]. 
Most common reason is that no connect operation (ConnectTo[${D},${D}]) available and thus the memory would not be writable""")
trait Storable[D<:Data] { def writer: ConnectTo[D,D] }
// Only reason Storable trait exists is so that failing to create a Vec gives a specialized error message
object Storable {
  implicit def storer[D<:Data](implicit ev: ConnectTo[D,D]): Storable[D] = new Storable[D] {val writer = implicitly[ConnectTo[D,D]]}
}

object Mem {
  import scala.language.experimental.macros
  import gama.internal.macrodefs.{TransformMacro => XFORM}
  
  // External API
  def apply[D<:Data](model: D, depth: Int): Mem[D] = macro XFORM.doMem.xform[D]

  // external->internal API
  def doMem[D<:Data](model: D, depth: Int, storer: Storable[D], info: EnclosureInfo): Mem[D] = {
    val newmemory = new Mem(model.copy.rebind(MemSpecSpell(info.em)), depth, storer, info)
    info.em.getActiveJournal.append(CreateMem(newmemory))

    newmemory
  }
} // TODO: MEMORY WRITE MASKS?

final class Mem[D<:Data] private (protected[gama] val elemType: D, val depth: Int, storer: Storable[D], protected[gama] val info: EnclosureInfo) extends MemAccessible[D] with Nameable {
  def collection = this
  
  protected[gama] def lookupIsConnectable(selector: UIntLike): Boolean = true
  // TODO: REMOVE WHEN ACCESSIBLE REFACTORED? probably no, Accessible works w Mem now
  // TODO: CONSIDER: Return false so writes can only be done through write call. Otherwise can do myMemOfBundle(myAddr).myBundleField := myUpdate, which is strange.
  // However, that may be a good story for masked writes??

  def propogateName(newname: NameTree, newsource: NameSource): Unit = {} 

  // external API
  def write[From<:Data](addr: UIntLike, source: From)(implicit acc_em: EnclosingModule, writer: ConnectTo[D, From]): Unit = {
    if(acc_em != info.em) { throw CrossedMemoryAccessException(acc_em, info.em) }
    val accessor = makeAccessor(addr, EnclosureInfo(info.em, None)) // TODO: GET INFO
    writer.monoConnect(Sink(accessor), Source(source), info)
  }
}
