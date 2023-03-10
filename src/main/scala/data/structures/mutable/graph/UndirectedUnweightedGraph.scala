package data.structures.mutable.graph

/**
 * Interface for representing an undirected unweighted graph.
 *
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo
 */
trait UndirectedUnweightedGraph[V] extends UndirectedGraph[V] with UnweightedGraph[V] {
  /**
   * Adds an edge to a graph.
   *
   * @param edge edge to add to graph.
   * @return `true` if edge was not in graph before.
   */
  def addEdge(edge: Edge[V]): Unit

  /**
   * Checks whether an undirected unweighted edge is included in graph.
   *
   * @param edge undirected unweighted edge for which to check inclusion.
   * @return `true` if undirected unweighted edge is included in graph.
   */
  def containsEdge(edge: Edge[V]): Boolean

  /**
   * Deletes an undirected unweighted edge from graph.
   *
   * @param edge an undirected unweighted edge to delete.
   * @return `true` if undirected unweighted edge was in graph before.
   */
  def deleteEdge(edge: Edge[V]): Unit
}
