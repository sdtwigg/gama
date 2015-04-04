package gama

package internal {
  object unsafeFlags {
    sealed trait MIC
    object moduleInnerClasses extends MIC
  }
}

object unsafe {
  import internal.unsafeFlags
  implicit lazy val moduleInnerClasses = internal.unsafeFlags.moduleInnerClasses
}

