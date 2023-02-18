package graph

import scala.collection.{immutable, mutable}

/**
 * Interface for representing a graph.
 *
 * @tparam V type of vertices in graph.
 * @tparam E type constructor for edges in graph.
 * @author Pepe Gallardo
 */
trait Graph[V, E[_]] {
  /**
   * Adds a vertex to a graph.
   *
   * @param vertex vertex to add.
   */
  def addVertex(vertex: V): Unit

  /**
   * Deletes a vertex from a graph.
   *
   * @param vertex vertex to delete.
   */
  def deleteVertex(vertex: V): Unit

  /**
   * Checks whether a vertex is included in graph.
   *
   * @param vertex vertex to check for inclusion.
   * @return `true` if vertex is in graph.
   */
  def containsVertex(vertex: V): Boolean

  /**
   * Returns a set with all vertices in graph.
   *
   * @return a set with all vertices in graph.
   */
  def vertices: immutable.Set[V]

  /**
   * Returns number of vertices included in graph.
   *
   * @return number of vertices included in graph.
   */
  def order: Int

  /**
   * Returns a set with all vertices incident on vertex `vertex`.
   *
   * @param vertex endpoint for which we seek incident vertices.
   * @return a set with all vertices incident on vertex `vertex`.
   */
  def successors(vertex: V): immutable.Set[V]

  /**
   * Returns number of vertices incident on vertex `vertex`.
   *
   * @param vertex endpoint for which we seek its number of incident vertices.
   * @return number of vertices incident on vertex `vertex`.
   */
  def degree(vertex: V): Int

  /**
   * Adds an edge to graph connecting `vertex1` to `vertex2`.
   *
   * @param vertex1 one endpoint of edge.
   * @param vertex2 another endpoint of edge.
   * @return edge that was added to graph.
   */
  def addEdge(vertex1: V, vertex2: V): E[V]

  /**
   * Adds an edge to a graph.
   *
   * @param edge edge to add to graph.
   */
  def addEdge(edge: E[V]): Unit

  /**
   * Deletes an edge from graph.
   *
   * @param edge edge to delete from graph.
   */
  def deleteEdge(edge: E[V]): Unit

  /**
   * Checks whether an edge is included in graph.
   *
   * @param edge edge to check inclusion in graph for.
   * @return `true` if edge is included in graph.
   */
  def containsEdge(edge: E[V]): Boolean

  /**
   * Returns a set with all edges in graph.
   *
   * @return a set with all edges in graph.
   */
  def edges: immutable.Set[E[V]]

  /**
   * Returns number of edges in graph.
   *
   * @return number of edges in graph.
   */
  def size: Int
}


































