package data.structures.mutable.graph

import scala.collection.{immutable, mutable}

object MapDirectedWeightedGraph {
  /**
   * Constructs a directed weighted graph using a mutable map.
   *
   * @tparam V type of vertices in graph.
   * @tparam W type of weights in graph.          
   * @return a directed weighted graph using a mutable map.
   */
  def apply[V, W](): MapDirectedWeightedGraph[V, W] = new MapDirectedWeightedGraph()
}

/**
 * An implementation of directed weighted graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @tparam W type of weights in graph.          
 * @author Pepe Gallardo          
 */
class MapDirectedWeightedGraph[V, W] extends DirectedWeightedGraph[V, W, DirectedWeightedEdge] {
  private val succsAndWeights = mutable.Map[V, mutable.Set[Pair[V, W]]]()

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

  /**
   * Returns a set with all vertices that are direct successors of vertex `source` and weights of corresponding 
   * directed edge.
   *
   * @param source source vertex for which we seek direct successors.
   * @return a set with all vertices that are direct successors of vertex `source` and weights of corresponding 
   *         directed edge.
   */
  override def successorsAndWeights(source: V): immutable.Set[(V, W)] = ???

  override def predecessorsAndWeights(destination: V): immutable.Set[(V, W)] = ???

  override def degree(vertex: V): Int = ???

  override def indegree(destination: V): Int = ???

  override def outdegree(source: V): Int = ???

  /**
   * Adds a directed weighted edge to graph connecting `source` to `destination`. Weight would be `null`.
   *
   * @param source      source of directed edge to add.
   * @param destination destination of directed edge to add.
   * @return `true` if directed edge was not in graph.
   */
  override def addEdge(source: V, destination: V): Boolean = ???

  /**
   * Adds a directed weighted edge to graph connecting `source` to `destination` with weight `weight`.
   *
   * @param source      source of directed edge to add.
   * @param destination destination of directed edge to add.
   * @param weight      weight of directed edge to add.
   * @return `true` if directed edge was not in graph.
   */
  override def addEdge(source: V, destination: V, weight: W): Boolean = ???

  /**
   * Adds a directed weighted edge to graph.
   *
   * @param edge directed weighted edge to add to graph.
   * @return `true` if directed edge was not in graph.
   */
  override def addEdge(edge: DirectedWeightedEdge[V, W]): Boolean = ???

  /**
   * Deletes a directed weighted edge from `source` to `destination` from graph.
   *
   * @param source      source vertex of directed edge to delete.
   * @param destination destination vertex of directed edge to delete.
   * @return `true` if edge was in graph before.
   */
  def deleteEdge(source: V, destination: V): Boolean = ???

  /**
   * Deletes a directed weighted edge from graph.
   *
   * @param edge a directed weighted edge to delete from graph.
   * @return `true` if directed edge was in graph.
   */
  override def deleteEdge(edge: DirectedWeightedEdge[V, W]): Boolean = ???

  /**
   * Checks whether a directed edge is included in graph.
   *
   * @param source      source vertex of directed edge.
   * @param destination destination vertex of directed edge.
   * @return `true` if edge is included in graph.
   */
  override def containsEdge(source: V, destination: V): Boolean = ???

  /**
   * Returns weight of edge going form vertex `source` to vertex `destination`.
   *
   * @param source      source vertex of seek weighted edge.
   * @param destination destination vertex of seek weighted edge.
   * @return `Some(weight)` if there is and edge from vertex `vertex1` to vertex `vertex2`. `None` if there is
   *         not such edge.
   */
  override def weightOfEdge(source: V, destination: V): Option[W] =
    succsAndWeights.get(source) match
      case None => None
      case Some(incidents) => incidents.find(_.vertex == destination) match
        case None => None
        case Some(Pair(_, weight)) => Some(weight)

  /**
   * Checks whether a directed weighted edge is included in graph.
   *
   * @param edge directed weighted edge to check inclusion in graph for.
   * @return `true` if directed weighted edge is included in graph.
   */
  override def containsEdge(edge: DirectedWeightedEdge[V, W]): Boolean = ???

  /**
   * Returns a set with all directed weighted edges in graph.
   *
   * @return a set with all directed weigthed edges in graph.
   */
  override def edges: Set[DirectedWeightedEdge[V, W]] = ???

  /**
   * Returns number of directed weighted edges in graph.
   *
   * @return number of directed weighted edges in graph.
   */
  override def size: Int = ???
}