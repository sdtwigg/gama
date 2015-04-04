package gama
package internal

object Port {
  protected[gama] def apply[D<: Data](model: D, em: EnclosingModule): D = {
    model.rebind(PortSpell(em))
  }
}
