package data.structures.mutable.graph

import scala.collection.{immutable, mutable}

object MapWeightedGraph {
  /**
   * Constructs an undirected weighted graph using a mutable map.
   *
   * @tparam V type of vertices in graph.
   * @tparam W type of weights in graph.
   * @return an undirected weighted graph using a mutable map.
   */
  def apply[V, W](): MapWeightedGraph[V, W] = new MapWeightedGraph()
}

/**
 * An implementation of undirected weighted graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @tparam W type of weights in graph.
 * @author Pepe Gallardo
 */
class MapWeightedGraph[V, W] extends UndirectedWeightedGraph[V, W] {
  private val succsAndWeights = mutable.Map[V, mutable.Set[Pair[V, W]]]()

  override def addVertex(vertex: V): Boolean =
    succsAndWeights.get(vertex) match
      case None => succsAndWeights(vertex) = mutable.Set[Pair[V, W]]()
        true
      case Some(_) => false

  override def containsVertex(vertex: V): Boolean =
    succsAndWeights.isDefinedAt(vertex)

  override def deleteVertex(vertex: V): Boolean =
    succsAndWeights.get(vertex) match
      case None => false
      case Some(adjacents) =>
        succsAndWeights.remove(vertex)
        for (Pair(adjacent, _) <- adjacents)
          succsAndWeights(adjacent).remove(Pair(vertex))
        true

  override def vertices: immutable.Set[V] =
    var set = immutable.Set[V]()
    for (vertex <- succsAndWeights.keys)
      set = set + vertex
    set

  override def order: Int =
    succsAndWeights.keys.size

  override def addEdge(weightedEdge: WeightedEdge[V, W]): Unit =
    addEdge(weightedEdge.vertex1, weightedEdge.vertex2, weightedEdge.weight)

  override def addEdge(vertex1: V, vertex2: V, weight: W): Boolean =
    succsAndWeights.get(vertex1) match
      case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
      case Some(pairs1) => succsAndWeights.get(vertex2) match
        case None => throw GraphException(s"addEdge: vertex $vertex2 is not in graph")
        case Some(pairs2) =>
          // there must be a single edge connecting any two vertices
          val deleted = pairs1.remove(Pair(vertex2))
          if (deleted)
            pairs2.remove(Pair(vertex1))
          val added = pairs1.add(Pair(vertex2, weight))
          pairs2.add(Pair(vertex1, weight))
          added

  override def containsEdge(vertex1: V, vertex2: V): Boolean =
    succsAndWeights.get(vertex1) match
      case None => false
      case Some(pairs) => pairs.contains(Pair(vertex2))

  override def containsEdge(weightedEdge: WeightedEdge[V, W]): Boolean =
    containsEdge(weightedEdge.vertex1, weightedEdge.vertex2, weightedEdge.weight)

  override def containsEdge(vertex1: V, vertex2: V, weight: W): Boolean =
    succsAndWeights.get(vertex1) match
      case None => false
      case Some(pairs) => pairs.contains(Pair(vertex2, weight))

  override def deleteEdge(vertex1: V, vertex2: V): Boolean =
    succsAndWeights.get(vertex1) match
      case None => throw GraphException(s"deleteEdge: vertex $vertex1 is not in graph")
      case Some(pairs1) => succsAndWeights.get(vertex2) match
        case None => throw GraphException(s"deleteEdge: vertex $vertex2 is not in graph")
        case Some(pairs2) =>
          val deleted = pairs1.remove(Pair(vertex2))
          pairs2.remove(Pair(vertex1))
          deleted

  override def deleteEdge(weightedEdge: WeightedEdge[V, W]): Boolean =
    deleteEdge(weightedEdge.vertex1, weightedEdge.vertex2, weightedEdge.weight)

  override def deleteEdge(vertex1: V, vertex2: V, weight: W): Boolean =
    succsAndWeights.get(vertex1) match
      case None => throw GraphException(s"deleteEdge: vertex $vertex1 is not in graph")
      case Some(pairs1) => succsAndWeights.get(vertex2) match
        case None => throw GraphException(s"deleteEdge: vertex $vertex2 is not in graph")
        case Some(pairs2) =>
          val deleted = pairs1.remove(Pair(vertex2, weight))
          pairs2.remove(Pair(vertex1, weight))
          deleted

  override def edges[Edge[X] >: WeightedEdge[X, W]]: immutable.Set[Edge[V]] =
    var set = immutable.Set[Edge[V]]()
    for ((vertex1, adjacents) <- succsAndWeights)
      for (Pair(vertex2, weight) <- adjacents)
        set = set + WeightedEdge(vertex1, vertex2, weight)
    set

  override def size: Int =
    var numEdges = 0
    for (adjacents <- succsAndWeights.values)
      numEdges += adjacents.size
    numEdges / 2

  override def weightOfEdge(vertex1: V, vertex2: V): Option[W] =
    succsAndWeights.get(vertex1) match
      case None => None
      case Some(pairs) => pairs.find(_.vertex == vertex2) match
        case None => None
        case Some(Pair(_, weight)) => Some(weight)

  override def adjacents(vertex: V): immutable.Set[V] =
    succsAndWeights.get(vertex) match
      case None => throw GraphException(s"adjacents: vertex $vertex is not in graph")
      case Some(pairs) =>
        var set = immutable.Set[V]()
        for (Pair(adjacent, _) <- pairs)
          set = set + adjacent
        set

  override def incidents[Edge[X] >: WeightedEdge[X, W]](vertex: V): immutable.Set[Edge[V]] =
    succsAndWeights.get(vertex) match
      case None => throw GraphException(s"incidents: vertex $vertex is not in graph")
      case Some(pairs) =>
        var set = immutable.Set[Edge[V]]()
        for (Pair(adjacent, weight) <- pairs)
          set = set + WeightedEdge(vertex, adjacent, weight)
        set

  override def degree(vertex: V): Int =
    succsAndWeights.get(vertex) match
      case None => throw GraphException(s"degree: vertex $vertex is not in graph")
      case Some(pairs) => pairs.size

  override def incidentsFrom[Edge[X] >: WeightedEdge[X, W]](vertex: V): immutable.Set[Edge[V]] =
    succsAndWeights.get(vertex) match
      case None => throw GraphException(s"incidentsFrom: vertex $vertex is not in graph")
      case Some(pairs) =>
        var set = immutable.Set[Edge[V]]()
        for (Pair(adjacent, weight) <- pairs)
          set = set + WeightedEdge(vertex, adjacent, weight)
        set

  override def incidentsTo[Edge[X] >: WeightedEdge[X, W]](vertex: V): immutable.Set[Edge[V]] =
    succsAndWeights.get(vertex) match
      case None => throw GraphException(s"incidentsTo: vertex $vertex is not in graph")
      case Some(pairs) =>
        var set = immutable.Set[Edge[V]]()
        for (Pair(adjacent, weight) <- pairs)
          set = set + WeightedEdge(adjacent, vertex, weight)
        set
}
