package gama
import internal._

@annotation.implicitNotFound("""Cannot create Mem[${D}]. 
Most common reason is that no self-muxing operation (SelfMuxable[${D}]) available""")
trait Storable[D<:Data] { def muxer: SelfMuxable[D] }
// Only reason Storable trait exists is so that failing to create a Vec gives a specialized error message
object Storable {
  implicit def storer[D<:Data: SelfMuxable]: Storable[D] = new Storable[D] {val muxer = implicitly[SelfMuxable[D]]}
} // TODO: REMOVE DEPEDENCY ON SELFMUXABLE?
// TODO: IS THIS EVEN REQUIRED AS CAN STRAIGHT COPY MODEL IN MEM?

object Mem {
  type AnyMem = Mem[T] forSome {type T<:Data}
}

final class Mem[D<: Data: Storable](model: D, em: EnclosingModule) extends MemAccessible[D] with Nameable {
  val elemType: D = model.copy // TODO: BIND TO A SEALED, NON-SYNTH NODE
  
  def lookupIsConnectable(selector: UInt): Boolean = true
  // TODO: REMOVE WHEN ACCESSIBLE REFACTORED? probably no, Accessible works w Mem now

  def propogateName(newname: NameTree, newsource: NameSource): Unit = ???

  def collection = this

  em.getActiveJournal.append(CreateMem(this))
}
