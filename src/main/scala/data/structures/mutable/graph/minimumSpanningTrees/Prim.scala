package data.structures.mutable.graph.minimumSpanningTrees

import data.structures.mutable.graph.{MapWeightedGraph, WeightedEdge, WeightedGraph}
import data.structures.mutable.heap.IndexedMinHeap

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
  private val minSpanningTree = new MapWeightedGraph[V, W]()
  run()

  /**
   * Returns a weighted graph corresponding to minimum spanning tree.
   *
   * @return a weighted graph corresponding to minimum spanning tree.
   */
  def minimumSpanningTree: WeightedGraph[V, W, WeightedEdge] =
    minSpanningTree

  private def run(): Unit =
    val vertices = weightedGraph.vertices

    if (vertices.nonEmpty)
      // a spanning tree should have order of the graph minus 1 vertices
      val finalNumberOfEdges = weightedGraph.order - 1
      var currentNumberOfEdges = 0

      // take one vertex and add it to spanning tree
      val vertex = vertices.head
      minSpanningTree.addVertex(vertex)

      val priority = Ordering.by((weightedEdge: WeightedEdge[V, W]) => weightedEdge.weight)
      val priorityQueue = IndexedMinHeap[WeightedEdge[V, W]](weightedGraph.size)(using priority)

      // add to priority queue edges incident to vertex
      for ((incident, weight) <- weightedGraph.successorsAndWeights(vertex))
        if (!minSpanningTree.containsVertex(incident))
          priorityQueue.insert(WeightedEdge(vertex, incident, weight))

      while (priorityQueue.nonEmpty && currentNumberOfEdges < finalNumberOfEdges)
        val weightedEdge = priorityQueue.extractFirst()
        val vertex = weightedEdge.vertex2
        if (!minSpanningTree.containsVertex(vertex))
          // vertex not in spanning tree yet. This edge is the one leading to it (from a vertex in spanning tree)
          // with minimal cost, hence we add vertex and edge to spanning tree
          minSpanningTree.addVertex(vertex)
          minSpanningTree.addEdge(weightedEdge)
          currentNumberOfEdges += 1

          // compute alternative costs for all vertices incident to this one which are not yet in spanning tree and put
          // them in priority queue as they may improve previous known ones
          for ((incident, weight) <- weightedGraph.successorsAndWeights(vertex))
            if (!minSpanningTree.containsVertex(incident))
              priorityQueue.insert(WeightedEdge(vertex, incident, weight))

/*
package data.structures.mutable.graph.minimumSpanningTrees

import data.structures.mutable.graph.{MapWeightedGraph, WeightedEdge, WeightedGraph}
import data.structures.mutable.heap.IndexedHeap

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
  private val minSpanningTree = new MapWeightedGraph[V, W]()
  run()

  private def run(): Unit =
    val vertices = weightedGraph.vertices

    if (vertices.nonEmpty)
      // a spanning tree should have order of the graph minus 1 vertices
      val finalNumberOfEdges = weightedGraph.order - 1
      var currentNumberOfEdges = 0

      // take one vertex and add it to spanning tree
      val vertex = vertices.head
      minSpanningTree.addVertex(vertex)
      println(vertex)

      val priority = Ordering.by((weightedEdge: WeightedEdge[V, W]) => weightedEdge.weight)
      val priorityQueue = IndexedHeap[WeightedEdge[V, W]](weightedGraph.size)(using priority)

      for ((incident, weight) <- weightedGraph.successorsAndWeights(vertex))
        if (!minSpanningTree.containsVertex(incident))
          priorityQueue.insert(WeightedEdge(vertex, incident, weight))

      while (priorityQueue.nonEmpty && currentNumberOfEdges < finalNumberOfEdges)
        val weightedEdge = priorityQueue.extractFirst()
        val vertex1 = weightedEdge.vertex1
        val vertex2 = weightedEdge.vertex2
        val add = !minimumSpanningTree.containsVertex(vertex1) || !minSpanningTree.containsVertex(vertex2)

        if (!minSpanningTree.containsVertex(vertex1))
          // vertex1 not in spanning tree yet. This edge is the one leading to it (from a vertex1 in spanning tree)
          // with minimal cost, hence we add vertex1 and edge to spanning tree
          minSpanningTree.addVertex(vertex1)
          println(vertex1)

          // compute alternative costs for all vertices incident to this one which are not yet in spanning tree and put
          // them in priority queue as they may improve previous known ones
          for ((incident, weight) <- weightedGraph.successorsAndWeights(vertex1))
            if (!minSpanningTree.containsVertex(incident))
              priorityQueue.insert(WeightedEdge(vertex1, incident, weight))

        if (!minSpanningTree.containsVertex(vertex2))
          // vertex1 not in spanning tree yet. This edge is the one leading to it (from a vertex1 in spanning tree)
          // with minimal cost, hence we add vertex1 and edge to spanning tree
          minSpanningTree.addVertex(vertex2)
          println(vertex2)

          // compute alternative costs for all vertices incident to this one which are not yet in spanning tree and put
          // them in priority queue as they may improve previous known ones
          for ((incident, weight) <- weightedGraph.successorsAndWeights(vertex2))
            if (!minSpanningTree.containsVertex(incident))
              priorityQueue.insert(WeightedEdge(vertex2, incident, weight))

        if (add)
          minSpanningTree.addEdge(weightedEdge)
          println(weightedEdge)
          currentNumberOfEdges += 1
  /**
   * Returns a weighted graph corresponding to minimum spanning tree.
   *
   * @return a weighted graph corresponding to minimum spanning tree.
   */
  def minimumSpanningTree: WeightedGraph[V, W, WeightedEdge] =
    minSpanningTree

*/