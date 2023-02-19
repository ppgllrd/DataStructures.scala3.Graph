package graph

import scala.collection.{immutable, mutable}

object MapWeightedGraph {
  /**
   * Constructs an undirected weighted graph using a mutable map.
   *
   * @tparam V type of vertices in graph.
   * @tparam W type of weights in graph.          
   * @return an undirected weighted graph using a mutable map.
   */
  def apply[V, W](): MapWeightedGraph[V, W] = new MapWeightedGraph()
}

/**
 * An implementation of weighted graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @tparam W type of weights in graph.          
 * @author Pepe Gallardo          
 */
class MapWeightedGraph[V, W] extends WeightedGraph[V, W, WeightedEdge] {
  private val succsAndWeights = mutable.Map[V, mutable.Set[Pair[V, W]]]()

  override def addVertex(vertex: V): Boolean = ???

  override def deleteVertex(vertex: V): Boolean = ???

  override def containsVertex(vertex: V): Boolean = ???

  override def vertices: immutable.Set[V] = ???

  override def order: Int = ???

  override def successors(vertex: V): immutable.Set[V] = ???

  override def successorsAndWeights(vertex: V): immutable.Set[(V, W)] = ???

  override def degree(vertex: V): Int = ???

  /**
   * Adds a weighted edge to graph connecting `source` to `destination`. Weight would be `null`.
   *
   * @param vertex1 one endpoint of edge.
   * @param vertex2 another endpoint of edge.
   * @return `true` if directed edge was not in graph.
   */
  override def addEdge(vertex1: V, vertex2: V): Boolean = ???

  /**
   * Adds a weighted edge to graph connecting `source` to `destination` with weight `weight`.
   *
   * @param vertex1 one endpoint of edge.
   * @param vertex2 another endpoint of edge.
   * @param weight  weight if edge.
   * @return `true` if directed edge was not in graph.
   */
  override def addEdge(vertex1: V, vertex2: V, weight: W): Boolean = ???

  /**
   * Adds a weighted edge to a graph.
   *
   * @param edge weighted edge to add to graph.
   * @return `true` if directed edge was not in graph.
   */
  override def addEdge(edge: WeightedEdge[V, W]): Boolean = ???

  /**
   * Deletes a weighted edge from graph.
   *
   * @param edge weighted edge to delete from graph.
   * @return `true` if directed edge was in graph.
   */
  override def deleteEdge(edge: WeightedEdge[V, W]): Boolean = ???

  /**
   * Checks whether a weighted edge is included in graph.
   *
   * @param edge weighted edge to check inclusion in graph for.
   * @return `true` if weighted edge is included in graph.
   */
  override def containsEdge(edge: WeightedEdge[V, W]): Boolean = ???

  /**
   * Returns a set with all weighted edges in graph.
   *
   * @return a set with all weighted edges in graph.
   */
  override def edges: Set[WeightedEdge[V, W]] = ???

  /**
   * Returns number of weighted edges in graph.
   *
   * @return number of weighted edges in graph.
   */
  override def size: Int = ???
}