package graph.minimumSpanningTrees

import collection.mutable
import collection.mutable.heap.MinHeapMap
import graph.{MapWeightedGraph, WeightedEdge, WeightedGraph}

/**
 * Class for computing minimum spanning tree for a weighted graph using Prim's algorithm.
 *
 * @param weightedGraph the weighted graph.
 * @param ord           `Ordering` used for comparing weights of edges.
 * @tparam V type of vertices in weighted graph.
 * @tparam W type of weights in weighted graph.
 * @author Pepe Gallardo
 */
class Prim[V, W](weightedGraph: WeightedGraph[V, W, WeightedEdge])(using ord: Ordering[W]):
  private val minSpanningTree = new MapWeightedGraph[V, W]()
  run()

  private def run(): Unit =
    val vertices = weightedGraph.vertices

    if (vertices.nonEmpty)
      // take one vertex and add it to spanning tree
      val vertex = vertices.head
      minSpanningTree.addVertex(vertex)

      val priority = Ordering.by((weightedEdge: WeightedEdge[V, W]) => weightedEdge.weight)
      val priorityQueue = MinHeapMap[WeightedEdge[V, W], Unit](using priority)

      // add to priority queue edge incident to vertex with minimal cost
      val iterator = weightedGraph.successorsAndWeights(vertex).iterator
      if (iterator.hasNext)
        var (bestIncident, bestWeight) = iterator.next()
        while (iterator.hasNext)
          val (incident, weight) = iterator.next()
          if (ord.compare(weight, bestWeight) < 0)
            bestIncident = incident
            bestWeight = weight
        priorityQueue.insert(WeightedEdge(vertex, bestIncident, bestWeight))

      while (priorityQueue.nonEmpty)
        val weightedEdge = priorityQueue.deleteFirst()
        val vertex = weightedEdge.vertex2
        if (!minSpanningTree.containsVertex(vertex))
          // vertex not in spanning tree yet. This edge is the one leading to it (from a vertex in spanning tree)
          // with minimal cost, hence we add vertex and edge to spanning tree
          minSpanningTree.addVertex(vertex)
          minSpanningTree.addEdge(weightedEdge)

          // compute alternative costs for all vertices incident to this one which are not yet in spanning tree and put
          // them in priority queue as they may improve previous known ones
          for ((incident, weight) <- weightedGraph.successorsAndWeights(vertex))
            if (!minSpanningTree.containsVertex(incident))
              priorityQueue.insert(WeightedEdge(vertex, incident, weight))

  /**
   * Returns a weighted graph corresponding to minimum spanning tree.
   *
   * @return a weighted graph corresponding to minimum spanning tree.
   */
  def minimumSpanningTree: WeightedGraph[V, W, WeightedEdge] =
    minSpanningTree

