package data.structures.mutable.graph.bipartiteness

import data.structures.mutable.graph.UndirectedGraph

import scala.collection.mutable

object Bipartiteness {
  /**
   * Constructs an object for checking whether an undirected graph is bipartite.
   *
   * @param undirectedGraph the graph to check.
   * @tparam V type of vertices in graph.
   * @return an object for checking whether an undirected graph is bipartite.
   */
  def apply[V](undirectedGraph: UndirectedGraph[V]): Bipartiteness[V] = new Bipartiteness(undirectedGraph)
}

/**
 * Class for checking whether an undirected graph is bipartite.
 *
 * @param undirectedGraph the graph to check.
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo
 */
class Bipartiteness[V](undirectedGraph: UndirectedGraph[V]) {
  private val colors = mutable.Map[V, Boolean]()
  private var bipartite = true
  run()

  /**
   * Returns `true` if graph is bipartite.
   *
   * @return `true` if graph is bipartite.
   */
  def isBipartite: Boolean = bipartite

  private inline def visited(vertex: V): Boolean =
    colors.isDefinedAt(vertex)

  private def run(): Unit = {
    val iterator = undirectedGraph.vertices.iterator
    while (bipartite && iterator.hasNext) {
      val vertex = iterator.next()
      if (!visited(vertex)) {
        depthFirst(vertex, true)
      }
    }
  }

  private def depthFirst(vertex: V, color: Boolean): Unit = {
    colors(vertex) = color
    val iterator = undirectedGraph.adjacents(vertex).iterator
    while (bipartite && iterator.hasNext) {
      val adjacent = iterator.next()
      colors.get(adjacent) match
        case None =>
          depthFirst(adjacent, !color)
        case Some(adjacentColor) =>
          if (adjacentColor == color)
            bipartite = false
    }
  }
}
