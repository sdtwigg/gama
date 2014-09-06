package gama

object TypeSafe {
  // The scala implicit magic
  implicit class unidirectional[LT <: Data](left: LT) {
    def :=[RT <: Data](right: RT)(implicit evi: RT <:< LT) = {
      left.unsafeAssign(right)
    }
  }
  /*implicit class bidirectional[LT <: Data](left: LT) {
    def <>[RT <: Data](right: RT)(implicit evi: LT =:= RT) = {
      left.biassign(right)
    }
  }*/
}

