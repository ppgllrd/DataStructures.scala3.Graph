package data.structures.mutable.graph

import scala.collection.{immutable, mutable}

object MapDirectedGraph {
  /**
   * Constructs a directed unweighted graph using a mutable map.
   *
   * @tparam V type of vertices in graph.
   * @return a directed unweighted graph using a mutable map.
   */
  def apply[V](): MapDirectedGraph[V] = new MapDirectedGraph()
}

/**
 * An implementation of directed unweighted graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo
 */
class MapDirectedGraph[V] extends DirectedUnweightedGraph[V] {
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
      case Some(_) =>
        succs.remove(vertex)
        for (heads <- succs.values)
          heads.remove(vertex)
        true

  override def vertices: immutable.Set[V] =
    var set = immutable.Set[V]()
    for (vertex <- succs.keys)
      set = set + vertex
    set

  override def order: Int =
    succs.keys.size

  override def addEdge(source: V, destination: V): Boolean =
    succs.get(source) match
      case None => throw GraphException(s"addEdge: vertex $source is not in graph")
      case Some(heads) =>
        if(!succs.isDefinedAt(destination))
          throw GraphException(s"addEdge: vertex $destination is not in graph")
        heads.add(destination)

  override def addEdge(directedEdge: DirectedEdge[V]): Unit =
    addEdge(directedEdge.source, directedEdge.destination)

  override def containsEdge(source: V, destination: V): Boolean =
    succs.get(source) match
      case None => false
      case Some(heads) => heads.contains(destination)

  override def containsEdge(directedEdge: DirectedEdge[V]): Boolean =
    containsEdge(directedEdge.source, directedEdge.destination)

  override def deleteEdge(source: V, destination: V): Boolean =
    succs.get(source) match
      case None => throw GraphException(s"deleteEdge: vertex $source is not in graph")
      case Some(heads) =>
        if (!succs.isDefinedAt(destination))
          throw GraphException(s"deleteEdge: vertex $destination is not in graph")
        heads.remove(destination)

  override def deleteEdge(directedEdge: DirectedEdge[V]): Boolean =
    deleteEdge(directedEdge.source, directedEdge.destination)

  override def edges[Edge[X] >: DirectedEdge[X]]: immutable.Set[Edge[V]] =
    var set = immutable.Set[Edge[V]]()
    for ((tail, heads) <- succs)
      for (head <- heads)
        set = set + DirectedEdge(tail, head)
    set

  override def size: Int =
    var numEdges = 0
    for (heads <- succs.values)
      numEdges += heads.size
    numEdges

  override def successors(source: V): immutable.Set[V] =
    succs.get(source) match
      case None => throw GraphException(s"successors: vertex $source is not in graph")
      case Some(heads) =>
        var set = immutable.Set[V]()
        for (head <- heads)
          set = set + head
        set

  override def predecessors(destination: V): immutable.Set[V] =
    if (!succs.isDefinedAt(destination))
      throw GraphException(s"predecessors: vertex $destination is not in graph")
    var set = immutable.Set[V]()
    for ((tail, heads) <- succs)
      if (heads.contains(destination))
        set = set + tail
    set

  override def incidentsFrom[Edge[X] >: DirectedEdge[X]](source: V): immutable.Set[Edge[V]] =
    succs.get(source) match
      case None => throw GraphException(s"incidentsFrom: vertex $source is not in graph")
      case Some(heads) =>
        var set = immutable.Set[Edge[V]]()
        for (head <- heads)
          set = set + DirectedEdge(source, head)
        set

  override def incidentsTo[Edge[X] >: DirectedEdge[X]](destination: V): immutable.Set[Edge[V]] =
    if (!succs.isDefinedAt(destination))
      throw GraphException(s"incidentsTo: vertex $destination is not in graph")
    var set = immutable.Set[Edge[V]]()
    for ((tail, heads) <- succs)
      if (heads.contains(destination))
        set = set + DirectedEdge(tail, destination)
    set

  override def outdegree(source: V): Int =
    succs.get(source) match
      case None => throw GraphException(s"outdegree: vertex $source is not in graph")
      case Some(heads) => heads.size

  override def indegree(destination: V): Int =
    if (!succs.isDefinedAt(destination))
      throw GraphException(s"indegree: vertex $destination is not in graph")
    var incidents = 0
    for (heads <- succs.values)
      if (heads.contains(destination))
        incidents += 1
    incidents


}
