package data.structures.mutable.graph

/**
 * Interface for representing a directed unweighted graph.
 *
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo
 */
trait DirectedUnweightedGraph[V] extends DirectedGraph[V] with UnweightedGraph[V] {
  /**
   * Adds a directed edge to a graph.
   *
   * @param directedEdge directed edge to add to graph.
   * @return `true` if edge was not in graph before.
   */
  def addEdge(directedEdge: DirectedEdge[V]): Unit

  /**
   * Checks whether a directed unweighted edge is included in graph.
   *
   * @param directedEdge directed unweighted edge for which to check inclusion.
   * @return `true` if directed unweighted edge is included in graph.
   */
  def containsEdge(directedEdge: DirectedEdge[V]): Boolean

  /**
   * Deletes a directed unweighted edge from graph.
   *
   * @param directedEdge a directed unweighted edge to delete.
   * @return `true` if directed unweighted edge was in graph before.
   */
  def deleteEdge(directedEdge: DirectedEdge[V]): Unit
}
