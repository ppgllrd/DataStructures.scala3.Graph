package graph

import scala.collection.immutable

/**
 * Interface for representing a directed weighted graph.
 *
 * @tparam V  type of vertices in graph.
 * @tparam W  type of weights in graph.
 * @tparam WE type constructor for directed weighted edges in graph.
 * @author Pepe Gallardo
 */
trait DirectedWeightedGraph[V, W, WE[_, _]] extends DirectedGraph[V, [X] =>> WE[X, W]]
  with WeightedGraph[V, W, WE] {
  /**
   * Returns a set with all vertices that are direct predecessors of `destination` and weights of corresponding directed
   * weighted edges.
   *
   * @param destination destination for which we seek direct successors.
   * @return a set with all vertices that are direct predecessors of `destination` and weights of corresponding directed
   *         weighted edges.
   */
  def predecessorsAndWeights(destination: V): immutable.Set[(V, W)] = ???
}
