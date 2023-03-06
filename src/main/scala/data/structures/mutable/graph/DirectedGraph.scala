package data.structures.mutable.graph

import scala.collection.immutable

/**
 * Interface for representing a directed graph.
 *
 * @tparam V type of vertices in graph.
 * @tparam E type constructor for directed edges in graph.
 * @author Pepe Gallardo
 */
trait DirectedGraph[V, E[+_]] extends Graph[V, E] {
  /**
   * Returns a set with all vertices that are direct predecessors of vertex `destination`.
   *
   * @param destination destination vertex for which we seek direct predecessors.
   * @return a set with all vertices that are direct predecessors of vertex `destination`.
   */
  def predecessors(destination: V): immutable.Set[V]

  /**
   * Returns number of directed edges whose destination is vertex `destination`.
   *
   * @param destination vertex that is destination of counted directed edges.
   * @return number of directed edges whose destination is vertex `destination`.
   */
  def indegree(destination: V): Int

  /**
   * Returns number of directed edges whose source is vertex `source`.
   *
   * @param source vertex that is source of counted directed edges.
   * @return number of directed edges whose source is vertex `source`.
   */
  def outdegree(source: V): Int
}
