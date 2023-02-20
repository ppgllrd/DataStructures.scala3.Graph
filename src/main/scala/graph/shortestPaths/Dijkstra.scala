package graph.shortestPaths

import graph.{GraphException, WeightedGraph}

import scala.collection.mutable

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
  private case class CostAndSource(cost: W, source: V)

  private val optimalCostAndSource = mutable.Map[V, CostAndSource]()

  private val ordering = Ordering.by[CostAndSource, W](_.cost).reverse
  private val priorityQueue = mutable.PriorityQueue[CostAndSource]()(ordering)

  optimalCostAndSource(source) = CostAndSource(num.zero, source)
  priorityQueue.enqueue(CostAndSource(num.zero, source))

  while (priorityQueue.nonEmpty)
    val CostAndSource(cost, vertex) = priorityQueue.dequeue()
    val expand = optimalCostAndSource(vertex).cost == cost
    // This is first extraction of vertex from PQ, hence it corresponds to its optimal cost, which is already
    // recorded in optimalCostAndSource.
    // Now that we know optimal cost for vertex, let's compute alternative costs to its neighbours and
    // update if they improve current ones
    if (expand)
      for ((incident, weight) <- weightedGraph.successorsAndWeights(vertex))
        val newCost = num.plus(cost, weight)
        val improvement =
          optimalCostAndSource.get(incident).fold(true) { case CostAndSource(cost, _) => ord.compare(newCost, cost) < 0 }
        if (improvement)
          optimalCostAndSource(incident) = CostAndSource(newCost, vertex)
          priorityQueue.enqueue(CostAndSource(newCost, incident))

  /**
   * Returns cost of shortest path from vertex `source` to vertex `destination`.
   *
   * @param destination destination vertex of sought path.
   * @return cost of shortest path from vertex `source` to vertex `destination`.
   */
  def lowestCostTo(destination: V): W =
    optimalCostAndSource.get(destination) match
      case None => throw GraphException(s"optimalCostTo: vertex $destination cannot be reached from vertex $source")
      case Some(CostAndSource(cost, _)) => cost

  /**
   * Returns shortest path from vertex `source` to vertex `destination`.
   *
   * @param destination destination vertex of sought path.
   * @return shortest path from vertex `source` to vertex `destination`.
   */
  def shortestPathTo(destination: V): List[V] =
    optimalCostAndSource.get(destination) match
      case None => throw GraphException(s"optimalPathTo: vertex $destination cannot be reached from vertex $source")
      case Some(CostAndSource(_, src)) =>
        var path = List(destination)
        if (destination != source)
          path = src :: path
        while (path.head != source)
          path = optimalCostAndSource(path.head).source :: path
        path

