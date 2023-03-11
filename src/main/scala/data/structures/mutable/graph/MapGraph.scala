package data.structures.mutable.graph

import scala.collection.{immutable, mutable}

object MapGraph {
  /**
   * Constructs an undirected unweighted graph using a mutable map.
   *
   * @tparam V type of vertices in graph.
   * @return an undirected unweighted graph using a mutable map.
   */
  def apply[V](): MapGraph[V] = new MapGraph()
}

/**
 * An implementation of undirected unweighted graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo
 */
class MapGraph[V] extends UndirectedUnweightedGraph[V] {
  private val succs = mutable.Map[V, mutable.Set[V]]()

  override def addVertex(vertex: V): Boolean =
    succs.get(vertex) match
      case None => succs(vertex) = mutable.Set[V]()
        true
      case Some(_) => false

  override def containsVertex(vertex: V): Boolean =
    succs.isDefinedAt(vertex)

  override def deleteVertex(vertex: V): Boolean =
    succs.get(vertex) match
      case None => false
      case Some(adjacents) =>
        succs.remove(vertex)
        for (adjacent <- adjacents)
          succs(adjacent).remove(vertex)
        true

  override def vertices: immutable.Set[V] =
    var set = immutable.Set[V]()
    for (vertex <- succs.keys)
      set = set + vertex
    set

  override def order: Int =
    succs.keys.size

  override def addEdge(edge: Edge[V]): Unit =
    addEdge(edge.vertex1, edge.vertex2)

  override def addEdge(vertex1: V, vertex2: V): Boolean =
    succs.get(vertex1) match
      case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
      case Some(adjacents1) => succs.get(vertex2) match
        case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
        case Some(adjacents2) =>
          val added = adjacents1.add(vertex2)
          adjacents2.add(vertex1)
          added

  override def containsEdge(edge: Edge[V]): Boolean =
    containsEdge(edge.vertex1, edge.vertex2)

  override def containsEdge(vertex1: V, vertex2: V): Boolean =
    succs.get(vertex1) match
      case None => false
      case Some(adjacents) => adjacents.contains(vertex2)

  override def deleteEdge(edge: Edge[V]): Boolean =
    deleteEdge(edge.vertex1, edge.vertex2)

  override def deleteEdge(vertex1: V, vertex2: V): Boolean =
    succs.get(vertex1) match
      case None => throw GraphException(s"deleteEdge: vertex $vertex1 is not in graph")
      case Some(adjacents1) => succs.get(vertex2) match
        case None => throw GraphException(s"deleteEdge: vertex $vertex1 is not in graph")
        case Some(adjacents2) =>
          val deleted = adjacents1.remove(vertex2)
          adjacents2.remove(vertex1)
          deleted

  override def edges[E[X] >: Edge[X]]: immutable.Set[E[V]] =
    var set = immutable.Set[E[V]]()
    for ((vertex1, adjacents) <- succs)
      for (vertex2 <- adjacents)
        set = set + Edge(vertex1, vertex2)
    set

  override def size: Int =
    var numEdges = 0
    for (adjacents <- succs.values)
      numEdges += adjacents.size
    numEdges / 2

  override def adjacents(vertex: V): immutable.Set[V] =
    succs.get(vertex) match
      case None => throw GraphException(s"adjacents: vertex $vertex is not in graph")
      case Some(adjacents) =>
        var set = immutable.Set[V]()
        for (adjacent <- adjacents)
          set = set + adjacent
        set

  override def incidents[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] =
    succs.get(vertex) match
      case None => throw GraphException(s"incidents: vertex $vertex is not in graph")
      case Some(adjacents) =>
        var set = immutable.Set[E[V]]()
        for (adjacent <- adjacents)
          set = set + Edge(vertex, adjacent)
        set

  override def incidentsFrom[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] =
    succs.get(vertex) match
      case None => throw GraphException(s"incidentsFrom: vertex $vertex is not in graph")
      case Some(adjacents) =>
        var set = immutable.Set[E[V]]()
        for (adjacent <- adjacents)
          set = set + Edge(vertex, adjacent)
        set

  override def incidentsTo[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] =
    succs.get(vertex) match
      case None => throw GraphException(s"incidentsTo: vertex $vertex is not in graph")
      case Some(adjacents) =>
        var set = immutable.Set[E[V]]()
        for (adjacent <- adjacents)
          set = set + Edge(adjacent, vertex)
        set

  override def degree(vertex: V): Int =
    succs.get(vertex) match
      case None => throw GraphException(s"degree: vertex $vertex is not in graph")
      case Some(adjacents) => adjacents.size
}
