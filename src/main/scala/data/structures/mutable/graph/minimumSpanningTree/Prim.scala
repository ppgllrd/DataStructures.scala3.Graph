package data.structures.mutable.graph.minimumSpanningTree

import data.structures.mutable.graph.{MapWeightedGraph, UndirectedWeightedGraph, WeightedEdge}

import scala.collection.mutable

object Prim {
  /**
   * An object for computing minimum spanning tree for an undirected weighted graph using Prim's algorithm.
   *
   * @param undirectedWeightedGraph the undirected weighted graph.
   * @param ord                     `Ordering` used for comparing weights of edges.
   * @tparam V type of vertices in weighted graph.
   * @tparam W type of weights in weighted graph.
   * @return an object for computing minimum spanning tree for a weighted graph using Prim's algorithm.
   */
  def apply[V, W](undirectedWeightedGraph: UndirectedWeightedGraph[V, W])(using ord: Ordering[W]): Prim[V, W] =
    new Prim(undirectedWeightedGraph)(using ord)
}

/**
 * Class for computing minimum spanning tree for an undirected weighted graph using Prim's algorithm.
 *
 * @param undirectedWeightedGraph the weighted graph.
 * @param ord                     `Ordering` used for comparing weights of edges.
 * @tparam V type of vertices in weighted graph.
 * @tparam W type of weights in weighted graph.
 * @author Pepe Gallardo
 */
class Prim[V, W](undirectedWeightedGraph: UndirectedWeightedGraph[V, W])(using ord: Ordering[W]):
  private val minSpanningTree = new MapWeightedGraph[V, W]()
  run()

  /**
   * Returns a weighted graph corresponding to minimum spanning tree.
   *
   * @return a weighted graph corresponding to minimum spanning tree.
   */
  def minimumSpanningTree: UndirectedWeightedGraph[V, W] =
    minSpanningTree

  private def run(): Unit =
    val vertices = undirectedWeightedGraph.vertices

    if (vertices.nonEmpty)
      // a spanning tree should have order of the graph minus 1 vertices
      val finalNumberOfEdges = undirectedWeightedGraph.order - 1
      var currentNumberOfEdges = 0

      // scala.collection.mutable.PriorityQueue is a max priority queue so Ordering must be reversed
      val priority = Ordering.by((weightedEdge: WeightedEdge[V, W]) => weightedEdge.weight).reverse
      val priorityQueue = new scala.collection.mutable.PriorityQueue(using priority)

      // take one vertex and add it to spanning tree
      val vertex = vertices.head
      minSpanningTree.addVertex(vertex)

      // add to priority queue edges incident to vertex
      for (weightedEdge <- undirectedWeightedGraph.incidentsFrom(vertex))
        priorityQueue.enqueue(weightedEdge)

      while (priorityQueue.nonEmpty && currentNumberOfEdges < finalNumberOfEdges)
        val weightedEdge@WeightedEdge(_, vertex, _) = priorityQueue.dequeue()
        if (!minSpanningTree.containsVertex(vertex))
          // vertex not in spanning tree yet. This edge is the one leading to it (from a vertex in spanning tree)
          // with minimal cost, hence we add vertex and edge to spanning tree
          minSpanningTree.addVertex(vertex)
          minSpanningTree.addEdge(weightedEdge)
          currentNumberOfEdges += 1

          // compute alternative costs for all vertices incident from this one which are not yet in spanning tree and
          // put them in priority queue as they may improve previous known ones
          for (weightedEdge@WeightedEdge(_, incident, _) <- undirectedWeightedGraph.incidentsFrom(vertex))
            if (!minSpanningTree.containsVertex(incident))
              priorityQueue.enqueue(weightedEdge)

