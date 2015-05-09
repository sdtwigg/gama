package gama
package intermediate
package passes

object ExpandVSelect extends PassMerger(Vector(ExpandVSelectSink, ExpandVSelectSource))
