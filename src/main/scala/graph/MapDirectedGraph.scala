package graph

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

  override def addVertex(vertex: V): Unit = ???

  override def deleteVertex(vertex: V): Unit = ???

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
   * @param source      source vertex of directed edge.
   * @param destination destination vertex of directed edge.
   * @return directed edge that was added to graph.
   */
  override def addEdge(source: V, destination: V): DirectedEdge[V] = ???

  /**
   * Adds a directed edge to graph.
   *
   * @param edge directed edge to add to graph.
   */
  override def addEdge(edge: DirectedEdge[V]): Unit = ???

  /**
   * Deletes a directed edge from graph.
   *
   * @param edge a directed edge to delete from graph.
   */
  override def deleteEdge(edge: DirectedEdge[V]): Unit = ???

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