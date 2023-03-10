package data.structures.mutable.graph

/**
 * Interface for representing an undirected weighted graph.
 *
 * @tparam V type of vertices in graph.
 * @tparam W type of weights in graph.
 * @author Pepe Gallardo
 */
trait UndirectedWeightedGraph[V, W] extends UndirectedGraph[V] with Graph[V, [X] =>> WeightedEdge[X, W]]
  with WeightedGraph[V, W] {
  /**
   * Adds a weighted edge to a graph.
   *
   * @param weightedEdge weighted edge to add to graph.
   * @return `true` if edge was not in graph before.
   */
  def addEdge(weightedEdge: WeightedEdge[V, W]): Unit

  /**
   * Checks whether an undirected weighted edge is included in graph.
   *
   * @param weightedEdge undirected weighted edge for which to check inclusion.
   * @return `true` if undirected weighted edge is included in graph.
   */
  def containsEdge(weightedEdge: WeightedEdge[V, W]): Boolean

  /**
   * Deletes an undirected weighted edge from graph.
   *
   * @param weightedEdge undirected weighted edge to delete.
   * @return `true` if undirected weighted edge was in graph before.
   */
  def deleteEdge(weightedEdge: WeightedEdge[V, W]): Unit
}
