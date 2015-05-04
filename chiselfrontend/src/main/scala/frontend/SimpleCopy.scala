package gama
package frontend

import implementation._

trait SimpleCopy {
  self: Data => // although, should only be needed for HW Tuples really....

  def simplecopy: this.type
    // just the structural copies, the particulars of the constituent SPEC may be wrong

  def copy: this.type = {
    val reborn = simplecopy
    reborn.mimic(this, asSPEC = true)
    reborn
  }
}
