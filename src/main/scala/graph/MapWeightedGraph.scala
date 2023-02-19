package graph

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
 * An implementation of weighted graphs using a mutable map.
 *
 * @tparam V type of vertices in graph.
 * @tparam W type of weights in graph.          
 * @author Pepe Gallardo          
 */
class MapWeightedGraph[V, W] extends WeightedGraph[V, W, WeightedEdge] {
  private val succsAndWeights = mutable.Map[V, mutable.Set[Pair[V, W]]]()

  override def addVertex(vertex: V): Boolean =
    succsAndWeights.get(vertex) match
      case None => succsAndWeights(vertex) = mutable.Set[Pair[V, W]]()
        true
      case Some(_) => false

  override def deleteVertex(vertex: V): Boolean =
    succsAndWeights.get(vertex) match
      case None => false
      case Some(incidents) =>
        succsAndWeights.remove(vertex)
        for (Pair(incident, _) <- incidents)
          succsAndWeights(incident).remove(Pair(vertex, null.asInstanceOf[W]))
        true

  override def containsVertex(vertex: V): Boolean =
    succsAndWeights.isDefinedAt(vertex)

  override def vertices: immutable.Set[V] =
    var set = immutable.Set[V]()
    for (vertex <- succsAndWeights.keys)
      set = set + vertex
    set

  override def order: Int =
    succsAndWeights.keys.size

  override def successors(vertex: V): immutable.Set[V] =
    succsAndWeights.get(vertex) match
      case None => throw GraphException(s"successors: vertex $vertex is not in graph")
      case Some(incidents) =>
        var set = immutable.Set[V]()
        for (Pair(incident, _) <- incidents)
          set = set + incident
        set

  override def successorsAndWeights(vertex: V): immutable.Set[(V, W)] =
    succsAndWeights.get(vertex) match
      case None => throw GraphException(s"successors: vertex $vertex is not in graph")
      case Some(incidents) =>
        var set = immutable.Set[(V, W)]()
        for (Pair(incident, weight) <- incidents)
          set = set + ((incident, weight))
        set

  override def degree(vertex: V): Int =
    succsAndWeights.get(vertex) match
      case None => throw GraphException(s"degree: vertex $vertex is not in graph")
      case Some(incidents) => incidents.size

  /**
   * Adds a weighted edge to graph connecting `source` to `destination`. Weight would be `null`.
   *
   * @param vertex1 one endpoint of edge to add.
   * @param vertex2 another endpoint of edge to add.
   * @return `true` if directed edge was not in graph.
   */
  override def addEdge(vertex1: V, vertex2: V): Boolean =
    addEdge(vertex1, vertex2, null.asInstanceOf[W])

  /**
   * Adds a weighted edge to graph connecting `source` to `destination` with weight `weight`.
   *
   * @param vertex1 one endpoint of edge to add.
   * @param vertex2 another endpoint of edge to add.
   * @param weight  weight if edge to add.
   * @return `true` if directed edge was not in graph.
   */
  override def addEdge(vertex1: V, vertex2: V, weight: W): Boolean =
    succsAndWeights.get(vertex1) match
      case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
      case Some(incidents1) => succsAndWeights.get(vertex2) match
        case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
        case Some(incidents2) =>
          if (incidents2.exists(_.vertex == vertex2))
            // there must be a single edge connecting ant two vertices
            incidents1.filterInPlace(_.vertex == vertex2)
            incidents2.filterInPlace(_.vertex == vertex1)
          val added = incidents1.add(Pair(vertex2, weight))
          incidents2.add(Pair(vertex1, weight))
          added

  /**
   * Adds a weighted edge to a graph.
   *
   * @param weightedEdge weighted edge to add to graph.
   * @return `true` if directed edge was not in graph.
   */
  override def addEdge(weightedEdge: WeightedEdge[V, W]): Boolean =
    addEdge(weightedEdge.vertex1, weightedEdge.vertex2, weightedEdge.weight)

  /**
   * Deletes a weighted edge connecting `vertex1` and `vertex2` from graph.
   *
   * @param vertex1 one endpoint of edge to delete.
   * @param vertex2 another endpoint of edge to delete.
   * @return `true` if edge was in graph before.
   */
  override def deleteEdge(vertex1: V, vertex2: V): Boolean =
    succsAndWeights.get(vertex1) match
      case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
      case Some(incidents1) => succsAndWeights.get(vertex2) match
        case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
        case Some(incidents2) =>
          val deleted = incidents1.remove(Pair(vertex2, null.asInstanceOf[W]))
          incidents2.remove(Pair(vertex1, null.asInstanceOf[W]))
          deleted

  /**
   * Deletes a weighted edge from graph.
   *
   * @param edge weighted edge to delete from graph.
   * @return `true` if directed edge was in graph.
   */
  override def deleteEdge(edge: WeightedEdge[V, W]): Boolean =
    val vertex1 = edge.vertex1
    val vertex2 = edge.vertex2
    succsAndWeights.get(vertex1) match
      case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
      case Some(incidents1) => succsAndWeights.get(vertex2) match
        case None => throw GraphException(s"addEdge: vertex $vertex1 is not in graph")
        case Some(incidents2) =>
          val deleted = incidents1.remove(Pair(vertex2, edge.weight))
          incidents2.remove(Pair(vertex1, edge.weight))
          deleted

  override def containsEdge(vertex1: V, vertex2: V): Boolean =
    succsAndWeights.get(vertex1) match
      case None => false
      case Some(incidents) => incidents.contains(Pair(vertex2, null.asInstanceOf[W]))

  /**
   * Checks whether a weighted edge is included in graph.
   *
   * @param edge weighted edge to check inclusion in graph for.
   * @return `true` if weighted edge is included in graph.
   */
  override def containsEdge(edge: WeightedEdge[V, W]): Boolean =
    succsAndWeights.get(edge.vertex1) match
      case None => false
      case Some(incidents) => incidents.contains(Pair(edge.vertex2, edge.weight))

  /**
   * Returns weight of edge connecting vertex `vertex1` and vertex `vertex2`.
   *
   * @param vertex1 one endpoint of seek weighted edge.
   * @param vertex2 another endpoint of seek weighted edge.
   * @return `Some(weight)` if there is and edge connecting vertex `vertex1` and vertex `vertex2`. `None` if there is
   *         not such edge.
   */
  override def weightOfEdge(vertex1: V, vertex2: V): Option[W] =
    succsAndWeights.get(vertex1) match
      case None => None
      case Some(incidents) => incidents.find(_.vertex == vertex2) match
        case None => None
        case Some(Pair(_, weight)) => Some(weight)

  /**
   * Returns a set with all weighted edges in graph.
   *
   * @return a set with all weighted edges in graph.
   */
  override def edges: Set[WeightedEdge[V, W]] =
    var set = immutable.Set[WeightedEdge[V, W]]()
    for ((vertex1, incidents) <- succsAndWeights)
      for (Pair(vertex2, weight) <- incidents)
        set = set + WeightedEdge(vertex1, vertex2, weight)
    set

  /**
   * Returns number of weighted edges in graph.
   *
   * @return number of weighted edges in graph.
   */
  override def size: Int =
    var numEdges = 0
    for (incidents <- succsAndWeights.values)
      numEdges += incidents.size
    numEdges / 2
}