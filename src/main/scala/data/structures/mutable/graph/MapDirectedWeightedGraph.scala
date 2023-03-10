package data.structures.mutable.graph

import scala.collection.*

object MapDirectedWeightedGraph {
  /**
   * Constructs a directed weighted graph using a mutable map.
   *
   * @tparam V type of vertices in graph.
   * @tparam W type of weights in graph.
   * @return a directed weighted graph using a mutable map.
   */
  def apply[V, W](): MapDirectedWeightedGraph[V, W] = new MapDirectedWeightedGraph()
}

/**
 * An implementation of directed weighted graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @tparam W type of weights in graph.
 * @author Pepe Gallardo
 */
class MapDirectedWeightedGraph[V, W] extends DirectedWeightedGraph[V, W] {
  override def addVertex(vertex: V): Boolean = ???

  override def containsVertex(vertex: V): Boolean = ???

  override def deleteVertex(vertex: V): Boolean = ???

  override def vertices: immutable.Set[V] = ???

  override def order: Int = ???


  override def addEdge(source: V, destination: V, weight: W): Boolean = ???

  override def addEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Unit = ???

  override def containsEdge(source: V, destination: V): Boolean = ???

  override def containsEdge(source: V, destination: V, weight: W): Boolean = ???

  override def containsEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Boolean = ???

  override def deleteEdge(source: V, destination: V): Boolean = ???

  override def deleteEdge(source: V, destination: V, weight: W): Boolean = ???

  override def deleteEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Unit = ???

  override def edges[Edge[X] >: DirectedWeightedEdge[X, W]]: immutable.Set[Edge[V]] = ???

  override def size: Int = ???

  override def weightOfEdge(source: V, destination: V): Option[W] = ???


  override def successors(source: V): immutable.Set[V] = ???

  override def predecessors(destination: V): immutable.Set[V] = ???

  override def incidentsFrom[Edge[X] >: DirectedWeightedEdge[X, W]](source: V): immutable.Set[Edge[V]] = ???

  override def incidentsTo[Edge[X] >: DirectedWeightedEdge[X, W]](destination: V): immutable.Set[Edge[V]] = ???

  override def outdegree(source: V): Int = ???

  override def indegree(destination: V): Int = ???
}
