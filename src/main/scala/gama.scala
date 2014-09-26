package object gama {
  type TypeDAnyN[D[X<:Node[Y],Y<:NS],NS<:NodeStore] = D[_<:Node[NS],NS]
}
