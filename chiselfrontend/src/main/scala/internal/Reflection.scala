package gama
package internal

import scala.reflect.runtime.{universe=>ru}

object Reflection {
  def getInstanceMirror(x: Any): ru.InstanceMirror =
    ru.runtimeMirror(x.getClass.getClassLoader).reflect(x)
  def getType(x: Any): ru.Type = 
    ru.runtimeMirror(x.getClass.getClassLoader).classSymbol(x.getClass).toType
}
