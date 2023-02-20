package graph.minimumSpanningTrees

import graph.{MapWeightedGraph, WeightedEdge, WeightedGraph}

import scala.collection.mutable

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
  private val vertices = weightedGraph.vertices
  private val minSpanningTree = new MapWeightedGraph[V, W]()

  private case class Best(vertex: V, weight: W)

  if (vertices.nonEmpty)
    // take one vertex and add it to spanning tree
    val vertex = vertices.head
    minSpanningTree.addVertex(vertex)

    val ordering = Ordering.by[WeightedEdge[V, W], W](_.weight).reverse
    val priorityQueue = new mutable.PriorityQueue[WeightedEdge[V, W]]()(ordering)

    // add to priority queue edge incident to vertex with minimal cost
    val iterator = weightedGraph.successorsAndWeights(vertex).iterator
    if (iterator.hasNext)
      val (incident, weight) = iterator.next()
      var best = Best(incident, weight)
      while (iterator.hasNext)
        val (incident, weight) = iterator.next()
        if (ord.compare(weight, best.weight) < 0)
          best = Best(incident, weight)
      priorityQueue.enqueue(WeightedEdge(vertex, best.vertex, best.weight))

    while (priorityQueue.nonEmpty)
      val edge = priorityQueue.dequeue()
      val vertex = edge.vertex2
      if (!minSpanningTree.containsVertex(vertex))
        // vertex not in spanning tree yet. This edge is the one leading to it (from a vertex in spanning tree)
        // with minimal cost, hence we add vertex and edge to spanning tree
        minSpanningTree.addVertex(vertex)
        minSpanningTree.addEdge(edge)

        // compute alternative costs for all vertices incident to this one which are not yet in spanning tree and put
        // them in priority queue as they may improve previous known ones
        for ((incident, weight) <- weightedGraph.successorsAndWeights(vertex))
          if (!minSpanningTree.containsVertex(incident))
            priorityQueue.enqueue(WeightedEdge(vertex, incident, weight))

  /**
   * Returns a weighted graph corresponding to minimum spanning tree.
   *
   * @return a weighted graph corresponding to minimum spanning tree.
   */
  def minimumSpanningTree: WeightedGraph[V, W, WeightedEdge] =
    minSpanningTree

