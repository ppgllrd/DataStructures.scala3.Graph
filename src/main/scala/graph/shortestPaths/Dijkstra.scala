package graph.shortestPaths

import collection.mutable
import graph.{GraphException, WeightedGraph}

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
  private final case class VertexAndCost(vertex: V, cost: W):
    def canEqual(other: Any): Boolean = other.isInstanceOf[VertexAndCost]

    // note that equality only considers vertex in structure but not its cost
    override def equals(other: Any): Boolean = other match
      case that: VertexAndCost =>
        (that canEqual this) && vertex == that.vertex
      case _ => false

    override def hashCode(): Int =
      vertex.hashCode()

  private def withKey(vertex: V) = VertexAndCost(vertex, null.asInstanceOf[W])

  private val priority = Ordering.by((vertexAndCost: VertexAndCost) => vertexAndCost.cost)
  private val priorityQueue = mutable.MinHeapMap[VertexAndCost, VertexAndCost](weightedGraph.order)(using priority)
  run()

  private def run(): Unit =
    // map is going to store for each vertex cost of best known path and vertex before in such path
    val sourcesAndCosts = priorityQueue.map

    val sourceAndCost = VertexAndCost(source, num.zero)
    // insert in heap source vertex and its optimal path cost (which is zero)
    val locatorSource = priorityQueue.insert(sourceAndCost)
    // record in map best known solution for vertex source
    sourcesAndCosts(locatorSource) = sourceAndCost

    while (priorityQueue.nonEmpty)
      val vertexAndCost@VertexAndCost(vertex, cost) = priorityQueue.deleteFirst()
      val expand = sourcesAndCosts(vertexAndCost).cost == cost
      // If cost of element extracted from heap is the same best known cost for that vertex, this is
      // first extraction of vertex from PQ, hence it corresponds to its optimal cost, which
      // is already recorded in the map.
      // Now that we know optimal cost for vertex, let's compute alternative costs to its neighbours and
      // update if they improve current ones
      if (expand)
        for ((incident, weight) <- weightedGraph.successorsAndWeights(vertex))
          val incidentLocator = priorityQueue.locatorFor(withKey(incident))
          val newCost = num.plus(cost, weight)
          // if incident is not yet in heap or if new cost is better this is an improvement
          val improvement =
            sourcesAndCosts.get(incidentLocator)
              .fold(true) { case VertexAndCost(_, cost) => ord.compare(newCost, cost) < 0 }
          if (improvement)
            // insert improved cost for incident
            priorityQueue.insert(incidentLocator, VertexAndCost(incident, newCost))
            // record in map this is now best known solution for incident and we are coming from vertex
            // priorityQueue.map(incidentLocator) = VertexAndCost(vertex, newCost)
            sourcesAndCosts(incidentLocator) = VertexAndCost(vertex, newCost)

  /**
   * Returns cost of shortest path from vertex `source` to vertex `destination`.
   *
   * @param destination destination vertex of sought path.
   * @return cost of shortest path from vertex `source` to vertex `destination`.
   */
  def lowestCostTo(destination: V): W =
    priorityQueue.map.get(withKey(destination)) match
      case None => throw GraphException(s"optimalCostTo: vertex $destination cannot be reached from vertex $source")
      case Some(VertexAndCost(_, cost)) => cost

  /**
   * Returns shortest path from vertex `source` to vertex `destination`.
   *
   * @param destination destination vertex of sought path.
   * @return shortest path from vertex `source` to vertex `destination`.
   */
  def shortestPathTo(destination: V): List[V] =
    priorityQueue.map.get(withKey(destination)) match
      case None => throw GraphException(s"optimalPathTo: vertex $destination cannot be reached from vertex $source")
      case Some(VertexAndCost(src, _)) =>
        var path = List(destination)
        if (destination != source)
          path = src :: path
        while (path.head != source)
          path = priorityQueue.map(withKey(path.head)).vertex :: path
        path

