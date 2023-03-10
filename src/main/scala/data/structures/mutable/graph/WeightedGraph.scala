package data.structures.mutable.graph

/**
 * Interface for representing a weighted graph.
 *
 * @tparam V type of vertices in graph.
 * @tparam W type of weights in graph.
 * @author Pepe Gallardo
 */
trait WeightedGraph[V, W] extends Graph[V, [X] =>> IsWeightedEdge[X, W]] {
  /**
   * Adds a weighted edge to graph connecting `vertex1` to `vertex2` with weight `weight`.
   *
   * @param vertex1 one endpoint of weighted edge to add.
   * @param vertex2 another endpoint of weighted edge to add.
   * @param weight  weight of weighted edge to add.
   * @return `true` if weighted edge was not in graph before.
   */
  def addEdge(vertex1: V, vertex2: V, weight: W): Boolean

  /**
   * Deletes a weighted edge from graph.
   *
   * @param vertex1 one endpoint of weighted edge to delete.
   * @param vertex2 another endpoint of weighted edge to delete.
   * @param weight  weight of weighted edge to delete.
   * @return `true` if weighted edge was in graph before.
   */
  def deleteEdge(vertex1: V, vertex2: V, weight: W): Boolean

  /**
   * Checks whether a weighted edge is included in graph.
   *
   * @param vertex1 one endpoint of weighted edge to check.
   * @param vertex2 another endpoint of weighted edge to check.
   * @param weight  weight of weighted edge to check.
   * @return `true` if weighted edge is included in graph.
   */
  def containsEdge(vertex1: V, vertex2: V, weight: W): Boolean

  /**
   * Returns weight of a weighted edge which is included in graph.
   *
   * @param vertex1 one endpoint of weighted edge.
   * @param vertex2 another endpoint of weighted edge.
   * @return `Some(weight)` if weighted edge is in graph and its weight is `weight` or `None` if weighted edge is not
   *         in graph.
   */
  def weightOfEdge(vertex1: V, vertex2: V): Option[W]
}
