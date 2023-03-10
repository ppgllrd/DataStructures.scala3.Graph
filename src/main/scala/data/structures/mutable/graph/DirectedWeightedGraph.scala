package data.structures.mutable.graph

/**
 * Interface for representing a directed weighted graph.
 *
 * @tparam V type of vertices in graph.
 * @tparam W type of weights in graph.
 * @author Pepe Gallardo
 */
trait DirectedWeightedGraph[V, W] extends DirectedGraph[V] with Graph[V, [X] =>> DirectedWeightedEdge[X, W]]
  with WeightedGraph[V, W] {
  /**
   * Adds a directed weighted edge to a graph.
   *
   * @param directedWeightedEdge directed weighted edge to add to graph.
   * @return `true` if edge was not in graph before.
   */
  def addEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Unit

  /**
   * Checks whether an directed weighted edge is included in graph.
   *
   * @param directedWeightedEdge directed weighted edge for which to check inclusion.
   * @return `true` if directed weighted edge is included in graph.
   */
  def containsEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Boolean

  /**
   * Deletes a directed weighted edge from graph.
   *
   * @param directedWeightedEdge directed weighted edge to delete.
   * @return `true` if directed weighted edge was in graph before.
   */
  def deleteEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Unit
}
