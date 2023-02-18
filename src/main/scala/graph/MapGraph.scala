package graph

import scala.collection.{immutable, mutable}

object MapGraph {
  /**
   * Constructs an undirected graph using a mutable map.
   *
   * @tparam V type of vertices in graph.
   * @return an undirected graph using a mutable map.
   */
  def apply[V](): MapGraph[V] = new MapGraph()
}

/**
 * An implementation of undirected graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo          
 */
class MapGraph[V] extends Graph[V, Edge] {
  private val succs = mutable.Map[V, mutable.Set[V]]()

  override def addVertex(vertex: V): Unit = ???

  override def deleteVertex(vertex: V): Unit = ???

  override def containsVertex(vertex: V): Boolean = ???

  override def vertices: immutable.Set[V] = ???

  override def order: Int = ???

  override def successors(vertex: V): immutable.Set[V] = ???

  override def degree(vertex: V): Int = ???

  override def addEdge(vertex1: V, vertex2: V): Edge[V] = ???

  override def addEdge(edge: Edge[V]): Unit = ???

  override def deleteEdge(edge: Edge[V]): Unit = ???

  override def containsEdge(edge: Edge[V]): Boolean = ???

  override def edges: Set[Edge[V]] = ???

  override def size: Int = ???
}