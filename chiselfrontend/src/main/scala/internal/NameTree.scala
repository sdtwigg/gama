package gama
package internal

sealed abstract class NameTree

case class NameTerm(identifier: String) extends NameTree
case class NameField(source: NameTree, field: String) extends NameTree
case class NameIndex(source: NameTree, index: Int) extends NameTree

case class NameLit[D<:Data](litdesc: LitDesc[D]) extends NameTree
case class NameUnnamedOp(opdesc: OpDesc) extends NameTree

case object NameUNKNOWN extends NameTree
