package data.structures.mutable.graph

import scala.collection.immutable

/**
 * Interface for representing an undirected graph.
 *
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo
 */
trait UndirectedGraph[V] extends Graph[V, Edge] {
  /**
   * Returns a set with all vertices adjacent to vertex `vertex`.
   *
   * @param vertex vertex for which we seek adjacent vertices.
   * @return a set with all vertices adjacent to vertex `vertex`.
   */
  def adjacents(vertex: V): immutable.Set[V]

  override def successors(vertex: V): immutable.Set[V] = adjacents(vertex)

  override def predecessors(vertex: V): immutable.Set[V] = adjacents(vertex)

  /**
   * Returns a set with all edges incident from vertex `vertex`.
   *
   * @param vertex starting vertex for which we seek incident from edges.
   * @return a set with all edges incident from vertex `vertex`.
   */
  def incidents[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] =
    incidentsFrom(vertex)

  /**
   * Returns number of edges incident on vertex `vertex`.
   *
   * @param vertex endpoint for which we seek its number of incident edges.
   * @return number of edges incident on vertex `vertex`.
   */
  def degree(vertex: V): Int
}
