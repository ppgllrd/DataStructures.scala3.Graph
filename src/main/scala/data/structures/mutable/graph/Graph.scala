package data.structures.mutable.graph

import scala.collection.immutable

/**
 * Interface for representing a graph.
 *
 * @tparam V type of vertices in graph.
 * @tparam E type constructor for edges in graph.
 * @author Pepe Gallardo
 */
trait Graph[V, +E[_]] extends data.structures.mutable.graph.traversal.Traversable[V] {
  /**
   * Adds a vertex to a graph.
   *
   * @param vertex vertex to add.
   * @return `true` if vertex was not in graph before.
   */
  def addVertex(vertex: V): Boolean

  /**
   * Deletes a vertex from a graph.
   *
   * @param vertex vertex to delete.
   * @return `true` if vertex was in graph before.
   */
  def deleteVertex(vertex: V): Boolean

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
   * Returns a set with all edges in graph.
   *
   * @return a set with all edges in graph.
   */
  def edges[Edge[X] >: E[X]]: immutable.Set[Edge[V]]

  /**
   * Returns number of edges in graph.
   *
   * @return number of edges in graph.
   */
  def size: Int

  /**
   * Deletes an edge from graph.
   *
   * @param vertex1 one endpoint of edge to delete.
   * @param vertex2 another endpoint of edge to delete.
   * @return `true` if edge was in graph before.
   */
  def deleteEdge(vertex1: V, vertex2: V): Boolean

  /**
   * Checks whether an edge is included in graph.
   *
   * @param vertex1 one endpoint of edge.
   * @param vertex2 another endpoint of edge.
   * @return `true` if edge is included in graph.
   */
  def containsEdge(vertex1: V, vertex2: V): Boolean

  /**
   * Returns a set with all vertices incident from vertex `vertex`.
   *
   * @param vertex starting vertex for which we seek incident vertices.
   * @return a set with all vertices incident from vertex `vertex`.
   */
  def successors(vertex: V): immutable.Set[V]

  /**
   * Returns a set with all vertices incident to vertex `vertex`.
   *
   * @param vertex ending vertex for which we seek incident vertices.
   * @return a set with all vertices incident on vertex `vertex`.
   */
  def predecessors(vertex: V): immutable.Set[V]

  /**
   * Returns a set with all edges incident from vertex `vertex`.
   *
   * @param vertex starting vertex for which we seek incident from edges.
   * @return a set with all edges incident from vertex `vertex`.
   */
  def incidentsFrom[Edge[X] >: E[X]](vertex: V): immutable.Set[Edge[V]]

  /**
   * Returns a set with all edges incident to vertex `vertex`.
   *
   * @param vertex ending vertex for which we seek incident to edges.
   * @return a set with all edges incident to vertex `vertex`.
   */
  def incidentsTo[Edge[X] >: E[X]](vertex: V): immutable.Set[Edge[V]]
}
