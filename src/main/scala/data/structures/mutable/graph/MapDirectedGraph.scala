package data.structures.mutable.graph

import scala.collection.{immutable, mutable}

object MapDirectedGraph {
  /**
   * Constructs a directed graph using a mutable map.
   *
   * @tparam V type of vertices in graph.
   * @return a directed graph using a mutable map.
   */
  def apply[V](): MapDirectedGraph[V] = new MapDirectedGraph()
}

/**
 * An implementation of directed graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo          
 */
class MapDirectedGraph[V] extends DirectedGraph[V, DirectedEdge] {
  private val succs = mutable.Map[V, mutable.Set[V]]()

  override def addVertex(vertex: V): Boolean = ???

  override def deleteVertex(vertex: V): Boolean = ???

  override def containsVertex(vertex: V): Boolean = ???

  override def vertices: immutable.Set[V] = ???

  override def order: Int = ???

  /**
   * Returns a set with all vertices that are direct successors of vertex `source`.
   *
   * @param source source vertex for which we seek direct successors.
   * @return a set with all vertices that are direct successors of vertex `source`.
   */
  override def successors(source: V): immutable.Set[V] = ???

  override def predecessors(destination: V): immutable.Set[V] = ???

  override def degree(vertex: V): Int = ???

  override def indegree(destination: V): Int = ???

  override def outdegree(source: V): Int = ???

  /**
   * Adds a directed edge to graph connecting vertex `source` to vertex `destination`.
   *
   * @param source      source vertex of directed edge to add.
   * @param destination destination vertex of directed edgeto add.
   * @return `true` if directed edge was not in graph.
   */
  override def addEdge(source: V, destination: V): Boolean = ???

  /**
   * Adds a directed edge to graph.
   *
   * @param edge directed edge to add to graph.
   * @return `true` if directed edge was not in graph.
   */
  override def addEdge(edge: DirectedEdge[V]): Boolean = ???

  /**
   * Deletes a directed edge from graph.
   *
   * @param source      source vertex of directed edge to delete.
   * @param destination destination vertex of directed edge to delete.
   * @return `true` if edge was in graph before.
   */
  def deleteEdge(source: V, destination: V): Boolean = ???

  /**
   * Deletes a directed edge from graph.
   *
   * @param edge a directed edge to delete from graph.
   * @return `true` if directed edge was in graph.
   */
  override def deleteEdge(edge: DirectedEdge[V]): Boolean = ???

  /**
   * Checks whether a directed edge is included in graph.
   *
   * @param source      source vertex of directed edge.
   * @param destination destination vertex of directed edge.
   * @return `true` if edge is included in graph.
   */
  override def containsEdge(source: V, destination: V): Boolean = ???

  /**
   * Checks whether a directed edge is included in graph.
   *
   * @param edge directed edge to check inclusion in graph for.
   * @return `true` if directed edge is included in graph.
   */
  override def containsEdge(edge: DirectedEdge[V]): Boolean = ???

  /**
   * Returns a set with all directed edges in graph.
   *
   * @return a set with all directed edges in graph.
   */
  override def edges: immutable.Set[DirectedEdge[V]] = ???

  /**
   * Returns number of directed edges in graph.
   *
   * @return number of directed edges in graph.
   */
  override def size: Int = ???
}