package gama
package internal

// OpId Here:
sealed trait OpId

//UNARY OPERATIONS
sealed trait OpIdUnary extends OpId
case object OpIDENT  extends OpIdUnary // internal: IDENTITY OPERATION

case object OpToUInt extends OpIdUnary
case object OpToSInt extends OpIdUnary
case object OpAsUInt extends OpIdUnary
case object OpAsSInt extends OpIdUnary

case object OpNot    extends OpIdUnary

case object OpXorRed extends OpIdUnary

//BINARY OPERATIONS
sealed trait OpIdBinary extends OpId
// Arithmetic
case object OpPlus  extends OpIdBinary
case object OpSubt  extends OpIdBinary
case object OpMult  extends OpIdBinary
case object OpDiv   extends OpIdBinary
case object OpMod   extends OpIdBinary
// Logical/Bitwise
case object OpAnd   extends OpIdBinary
case object OpOr    extends OpIdBinary
case object OpXor   extends OpIdBinary
// Bit fiddling
case object OpPadTo extends OpIdBinary
case object OpCat   extends OpIdBinary
case object OpLShft extends OpIdBinary
case object OpRShft extends OpIdBinary
// Comparison
case object OpEqual extends OpIdBinary
case object OpNotEq extends OpIdBinary
case object OpLess  extends OpIdBinary
case object OpLeEq  extends OpIdBinary
case object OpGrt   extends OpIdBinary
case object OpGrEq  extends OpIdBinary
