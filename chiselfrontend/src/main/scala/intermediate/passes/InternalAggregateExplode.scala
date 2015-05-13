package gama
package intermediate
package passes

object InternalAggregateExplode extends PassMerger(Vector(
  ConnectAggregateExplode, DeclAggregateExplode
))

