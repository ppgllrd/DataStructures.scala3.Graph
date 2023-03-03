package graph

import data.structures.mutable.graph.MapWeightedGraph

import scala.io.Source

object ReadWeightedGraph {
  def apply(path: String): MapWeightedGraph[Int, Double] = {
    val source = Source.fromFile(path)
    val wg = source.getLines.flatMap(_.split("\\s+")).toList match {
      case order :: size :: numbers =>
        val wg = MapWeightedGraph[Int, Double]()
        for (vertex <- 0 until order.toInt)
          wg.addVertex(vertex)
        for (List(v1, v2, w) <- numbers.grouped(3)) {
          wg.addEdge(v1.toInt, v2.toInt, w.toDouble)
        }
        wg
      case _ => sys.error("wrong problem format")
    }
    source.close()
    wg
  }
}
