package gama
package internal

// OpId Here:
sealed trait OpId

sealed trait OpIdUnary extends OpId
case object OpToUInt  extends OpIdUnary

sealed trait OpIdBinary extends OpId
case object OpPlus   extends OpIdBinary
case object OpPad    extends OpIdBinary
