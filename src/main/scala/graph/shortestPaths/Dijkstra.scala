package graph.shortestPaths

import graph.{GraphException, WeightedGraph}

import collection.mutable

/**
 * Class for computing shortest path from one source vertex to rest of reachable vertices in a non-negatively weighted
 * graph using Dijkstra algorithm.
 *
 * @param weightedGraph the weighted graph.
 * @param source        the source vertex for constructing paths.
 * @param ord           `Ordering` used for comparing weights of edges.
 * @param num           `Numeric` used for adding weights of edges.
 * @tparam V  type of vertices in weighted graph.
 * @tparam W  type of weights in weighted graph.
 * @tparam WE type of weighted edges in graph.
 * @author Pepe Gallardo
 */
class Dijkstra[V, W, WE[_, _]](weightedGraph: WeightedGraph[V, W, WE], source: V)(using ord: Ordering[W], num: Numeric[W]):
  private final case class VertexAndCost(vertex: V, cost: W) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[VertexAndCost]
    // note that equality only considers vertex in structure but not its cost
    override def equals(other: Any): Boolean = other match {
      case that: VertexAndCost =>
        (that canEqual this) && vertex == that.vertex
      case _ => false
    }

    override def hashCode(): Int =
      vertex.hashCode()
  }

  private val optimalSourceAndCost = scala.collection.mutable.Map[V, VertexAndCost]()

  private val priority = Ordering.by[VertexAndCost, W](_.cost)
  private val priorityQueue = mutable.MinUpdatableHeap[VertexAndCost](weightedGraph.order)(using priority)

  optimalSourceAndCost(source) = VertexAndCost(source, num.zero)
  priorityQueue.insertOrIncreasePriority(VertexAndCost(source, num.zero))

  while (priorityQueue.nonEmpty)
    val VertexAndCost(vertex, cost) = priorityQueue.deleteFirst()
    val expand = optimalSourceAndCost(vertex).cost == cost
    // If expand is true, this is first extraction of vertex from PQ, hence it corresponds to its optimal cost, which
    // is already recorded in optimalSourceAndCost.
    // Now that we know optimal cost for vertex, let's compute alternative costs to its neighbours and
    // update if they improve current ones
    if (expand)
      for ((incident, weight) <- weightedGraph.successorsAndWeights(vertex))
        val newCost = num.plus(cost, weight)
        val improvement =
          optimalSourceAndCost.get(incident).fold(true){ case VertexAndCost(_, cost) => ord.compare(newCost, cost) < 0 }
        if (improvement)
          optimalSourceAndCost(incident) = VertexAndCost(vertex, newCost)
          priorityQueue.insertOrIncreasePriority(VertexAndCost(incident, newCost))

  /**
   * Returns cost of shortest path from vertex `source` to vertex `destination`.
   *
   * @param destination destination vertex of sought path.
   * @return cost of shortest path from vertex `source` to vertex `destination`.
   */
  def lowestCostTo(destination: V): W =
    optimalSourceAndCost.get(destination) match
      case None => throw GraphException(s"optimalCostTo: vertex $destination cannot be reached from vertex $source")
      case Some(VertexAndCost(_, cost)) => cost

  /**
   * Returns shortest path from vertex `source` to vertex `destination`.
   *
   * @param destination destination vertex of sought path.
   * @return shortest path from vertex `source` to vertex `destination`.
   */
  def shortestPathTo(destination: V): List[V] =
    optimalSourceAndCost.get(destination) match
      case None => throw GraphException(s"optimalPathTo: vertex $destination cannot be reached from vertex $source")
      case Some(VertexAndCost(src, _)) =>
        var path = List(destination)
        if (destination != source)
          path = src :: path
        while (path.head != source)
          path = optimalSourceAndCost(path.head).vertex :: path
        path

