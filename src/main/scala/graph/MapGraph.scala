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

  override def addVertex(vertex: V): Boolean =
    succs.get(vertex) match
      case None => succs(vertex) = mutable.Set[V]()
        true
      case Some(_) => false

  override def deleteVertex(vertex: V): Boolean =
    succs.get(vertex) match
      case None => false
      case Some(incidents) =>
        succs.remove(vertex)
        for (incident <- incidents)
          succs(incident) -= vertex
        true

  override def containsVertex(vertex: V): Boolean =
    succs.isDefinedAt(vertex)

  override def vertices: immutable.Set[V] =
    var set = immutable.Set[V]()
    for (vertex <- succs.keys)
      set = set + vertex
    set

  override def order: Int =
    succs.keys.size

  override def successors(vertex: V): immutable.Set[V] =
    succs.get(vertex) match
      case None => throw GraphException(s"successors: vertex $vertex is not in graph")
      case Some(incidents) =>
        var set = immutable.Set[V]()
        for (incident <- incidents)
          set = set + incident
        set

  override def degree(vertex: V): Int =
    succs.get(vertex) match
      case None => throw GraphException(s"degree: vertex $vertex is not in graph")
      case Some(incidents) => incidents.size

  override def addEdge(edge: Edge[V]): Boolean =
    addEdge(edge.vertex1, edge.vertex2)

  override def addEdge(vertex1: V, vertex2: V): Boolean =
    succs.get(vertex1) match
      case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
      case Some(incidents1) => succs.get(vertex2) match
        case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
        case Some(incidents2) =>
          val added = incidents1.add(vertex2)
          incidents2.add(vertex1)
          added

  override def deleteEdge(edge: Edge[V]): Boolean =
    val vertex1 = edge.vertex1
    val vertex2 = edge.vertex2
    succs.get(vertex1) match
      case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
      case Some(incidents1) => succs.get(vertex2) match
        case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
        case Some(incidents2) =>
          val deleted = incidents1.remove(vertex2)
          incidents2.remove(vertex1)
          deleted

  override def containsEdge(edge: Edge[V]): Boolean =
    succs.get(edge.vertex1) match
      case None => false
      case Some(incidents) => incidents.contains(edge.vertex2)

  override def edges: immutable.Set[Edge[V]] =
    var set = immutable.Set[Edge[V]]()
    for ((vertex1, incidents) <- succs)
      for (vertex2 <- incidents)
        set = set + Edge(vertex1, vertex2)
    set

  override def size: Int =
    var numEdges = 0
    for (incidents <- succs.values)
      numEdges += incidents.size
    numEdges / 2
}