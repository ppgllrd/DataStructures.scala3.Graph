package data.structures.mutable.graph

/**
 * Interface for representing a directed graph.
 *
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo
 */
trait DirectedGraph[V] extends Graph[V, DirectedEdge] {
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
