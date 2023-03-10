package data.structures.mutable.graph

/**
 * Interface for representing an unweighted graph.
 *
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo
 */
trait UnweightedGraph[V] extends Graph[V, IsEdge] {
  /**
   * Adds an edge to graph connecting `vertex1` to `vertex2`.
   *
   * @param vertex1 one endpoint of edge to add.
   * @param vertex2 another endpoint of edge to add.
   * @return `true` if edge was not in graph before.
   */
  def addEdge(vertex1: V, vertex2: V): Boolean
}
