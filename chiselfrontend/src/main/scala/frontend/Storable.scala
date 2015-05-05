package gama
package frontend

import implementation._

@annotation.implicitNotFound("""Cannot find Storable[${D}] capability (type class) as required for Mem, Wire, or Reg instantiation. 
Most common reason is that no self-connect operation is available (ConnectTo[${D},${D}]) and thus the node would not be usable""")
trait Storable[D<:Data] { def writer: ConnectTo[D,D] }
// Only reason Storable trait exists is so that failing to create a Vec gives a specialized error message
object Storable {
  implicit def storer[D<:Data](implicit ev: ConnectTo[D,D]): Storable[D] = new Storable[D] {val writer = implicitly[ConnectTo[D,D]]}
}
