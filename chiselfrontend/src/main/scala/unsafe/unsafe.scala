package gama
package object unsafe {
  import internal.unsafeFlags
  implicit lazy val moduleInnerClasses = internal.unsafeFlags.moduleInnerClasses
}

