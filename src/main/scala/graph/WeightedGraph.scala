package graph

import scala.collection.immutable

/**
 * Interface for representing a weighted graph.
 *
 * @tparam V  type of vertices in graph.
 * @tparam W  type of weights in graph.
 * @tparam WE type constructor for weighted edges in graph.
 * @author Pepe Gallardo
 */
trait WeightedGraph[V, W, WE[_, _]] extends Graph[V, [X] =>> WE[X, W]] {
  def addEdge(vertex1: V, vertex2: V, weight: W): WE[V, W]

  /**
   * Returns a set with all vertices that are direct successors of vertex `vertex` and weights of corresponding 
   * weighted edges.
   *
   * @param vertex source vertex for which we seek direct successors.
   * @return a set with all vertices that are direct successors of vertex `vertex` and weights of corresponding 
   *         weighted edges.
   */
  def successorsAndWeights(vertex: V): immutable.Set[(V, W)]
}
