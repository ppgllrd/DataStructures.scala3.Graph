package data.structures.mutable.graph

import scala.collection.immutable

object MapDirectedGraph {
  /**
   * Constructs a directed unweighted graph using a mutable map.
   *
   * @tparam V type of vertices in graph.
   * @return a directed unweighted graph using a mutable map.
   */
  def apply[V](): MapDirectedGraph[V] = new MapDirectedGraph()
}

/**
 * An implementation of directed unweighted graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo
 */
class MapDirectedGraph[V] extends DirectedUnweightedGraph[V] {
  override def addVertex(vertex: V): Boolean = ???

  override def containsVertex(vertex: V): Boolean = ???

  override def deleteVertex(vertex: V): Boolean = ???

  override def vertices: immutable.Set[V] = ???

  override def order: Int = ???


  override def addEdge(source: V, destination: V): Boolean = ???

  override def addEdge(directedEdge: DirectedEdge[V]): Unit = ???

  override def containsEdge(source: V, destination: V): Boolean = ???

  override def containsEdge(directedEdge: DirectedEdge[V]): Boolean = ???

  override def deleteEdge(source: V, destination: V): Boolean = ???

  override def deleteEdge(directedEdge: DirectedEdge[V]): Boolean = ???

  override def edges[Edge[X] >: DirectedEdge[X]]: immutable.Set[Edge[V]] = ???

  override def size: Int = ???


  override def successors(source: V): immutable.Set[V] = ???

  override def predecessors(destination: V): immutable.Set[V] = ???

  override def incidentsFrom[Edge[X] >: DirectedEdge[X]](source: V): immutable.Set[Edge[V]] = ???

  override def incidentsTo[Edge[X] >: DirectedEdge[X]](destination: V): immutable.Set[Edge[V]] = ???

  override def outdegree(source: V): Int = ???

  override def indegree(destination: V): Int = ???
}
