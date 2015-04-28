package gama
package internal

import scala.language.existentials

sealed abstract class NameTree

case class NameTerm(identifier: String) extends NameTree
case class NameIO(source: Module[_<:Data]) extends NameTree
case class NameField(source: HardwareTuple, field: String) extends NameTree
case class NameIndex(source: Vec[_<:Data], index: Int) extends NameTree

case class NameLit[D<:Data](litdesc: LitDesc[D]) extends NameTree

case object NameUNKNOWN extends NameTree
