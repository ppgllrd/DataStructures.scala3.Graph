package data.structures.mutable.graph

import scala.collection.immutable

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
 * An implementation of undirected weighted graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @tparam W type of weights in graph.
 * @author Pepe Gallardo
 */
class MapWeightedGraph[V, W] extends UndirectedWeightedGraph[V, W] {
  private var xs = List[WeightedEdge[V, W]]()

  override def addVertex(vertex: V): Boolean = ???

  override def containsVertex(vertex: V): Boolean = ???

  override def deleteVertex(vertex: V): Boolean = ???

  override def vertices: immutable.Set[V] = ???

  override def order: Int = ???


  override def addEdge(vertex1: V, vertex2: V, weight: W): Boolean = ???

  override def addEdge(weightedEdge: WeightedEdge[V, W]): Unit = ???

  override def containsEdge(vertex1: V, vertex2: V): Boolean = ???

  override def containsEdge(vertex1: V, vertex2: V, weight: W): Boolean = ???

  override def containsEdge(weightedEdge: WeightedEdge[V, W]): Boolean = ???

  override def deleteEdge(vertex1: V, vertex2: V): Boolean = ???

  override def deleteEdge(vertex1: V, vertex2: V, weight: W): Boolean = ???

  override def deleteEdge(weightedEdge: WeightedEdge[V, W]): Unit = ???

  override def edges[Edge[X] >: WeightedEdge[X, W]]: immutable.Set[Edge[V]] = ???

  override def size: Int = ???

  override def weightOfEdge(vertex1: V, vertex2: V): Option[W] = ???


  override def adjacents(vertex: V): immutable.Set[V] = ???

  override def incidents[Edge[X] >: WeightedEdge[X, W]](vertex: V): immutable.Set[Edge[V]] = ???

  override def degree(vertex: V): Int = ???

  override def incidentsFrom[Edge[X] >: WeightedEdge[X, W]](vertex: V): immutable.Set[Edge[V]] = ???

  override def incidentsTo[Edge[X] >: WeightedEdge[X, W]](vertex: V): immutable.Set[Edge[V]] = ???
}
