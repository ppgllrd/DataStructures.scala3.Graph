package data.structures.mutable.graph

import scala.collection.immutable

object MapGraph {
  /**
   * Constructs an undirected unweighted graph using a mutable map.
   *
   * @tparam V type of vertices in graph.
   * @return an undirected unweighted graph using a mutable map.
   */
  def apply[V](): MapGraph[V] = new MapGraph()
}

/**
 * An implementation of undirected unweighted graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo
 */
class MapGraph[V] extends UndirectedUnweightedGraph[V] {
  private var xs = List[Edge[V]]()

  override def addVertex(vertex: V): Boolean = ???

  override def containsVertex(vertex: V): Boolean = ???

  override def deleteVertex(vertex: V): Boolean = ???

  override def vertices: immutable.Set[V] = ???

  override def order: Int = ???


  override def addEdge(vertex1: V, vertex2: V): Boolean = ???

  override def addEdge(edge: Edge[V]): Unit = ???

  override def containsEdge(vertex1: V, vertex2: V): Boolean = ???

  override def containsEdge(edge: Edge[V]): Boolean = ???

  override def deleteEdge(vertex1: V, vertex2: V): Boolean = ???

  override def deleteEdge(edge: Edge[V]): Unit = ???

  override def edges[E[X] >: Edge[X]]: immutable.Set[E[V]] = ???

  override def size: Int = ???


  override def adjacents(vertex: V): immutable.Set[V] = ???

  override def incidents[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] = ???

  override def incidentsFrom[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] = ???

  override def incidentsTo[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] = ???

  override def degree(vertex: V): Int = ???
}
