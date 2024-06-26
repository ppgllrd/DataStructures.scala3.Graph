package data.structures.mutable.graph.minimumSpanningTree

import data.structures.mutable.disjointSet.DisjointSet
import data.structures.mutable.disjointSet.indexedSet.ArrayIndexedSet
import data.structures.mutable.graph.{MapWeightedGraph, UndirectedWeightedGraph, WeightedEdge}

import scala.reflect.ClassTag


object Kruskal {
  /**
   * An object for computing minimum spanning tree for an undirected weighted graph using Kruskal's algorithm.
   *
   * @param undirectedWeightedGraph the undirected weighted graph.
   * @param ord                     `Ordering` used for comparing weights of edges.
   * @tparam V type of vertices in weiauthor Pepe Gallardoghted graph.
   * @tparam W type of weights in weighted graph.
   * @param classTagV a class tag for type of vertices.
   * @return an object for computing minimum spanning tree for a weighted graph using Kruskal's algorithm.
   */
  def apply[V, W](undirectedWeightedGraph: UndirectedWeightedGraph[V, W])(using ord: Ordering[W])(using classTagV: ClassTag[V])
  : Kruskal[V, W] = new Kruskal(undirectedWeightedGraph)(using ord)(using classTagV)
}

/**
 * Class for computing minimum spanning tree for an undirected weighted graph using Kruskal's algorithm.
 *
 * @param undirectedWeightedGraph the undirected weighted graph.
 * @param ord                     `Ordering` used for comparing weights of edges.
 * @tparam V type of vertices in weighted graph.
 * @tparam W type of weights in weighted graph.
 * @param classTagV a class tag for type of vertices.
 * @author Pepe Gallardo
 */
class Kruskal[V, W](undirectedWeightedGraph: UndirectedWeightedGraph[V, W])(using ord: Ordering[W])(using classTagV: ClassTag[V]):
  private val minSpanningTree = new MapWeightedGraph[V, W]()
  run()

  /**
   * Returns a weighted graph corresponding to minimum spanning tree.
   *
   * @return a weighted graph corresponding to minimum spanning tree.
   */
  def minimumSpanningTree: UndirectedWeightedGraph[V, W] =
    minSpanningTree

  private def run(): Unit = {
    val vertices = undirectedWeightedGraph.vertices

    if (vertices.nonEmpty)
      // a spanning tree should have order of the graph minus 1 vertices
      val finalNumberOfEdges = undirectedWeightedGraph.order - 1
      var currentNumberOfEdges = 0

      val indexedSet = ArrayIndexedSet(undirectedWeightedGraph.vertices.toArray)
      val disjointSet = DisjointSet.fromIndexedSet(indexedSet)

      // scala.collection.mutable.PriorityQueue is a max priority queue so Ordering must be reversed
      val priority = Ordering.by((weightedEdge: WeightedEdge[V, W]) => weightedEdge.weight).reverse
      val priorityQueue = scala.collection.mutable.PriorityQueue.from(undirectedWeightedGraph.edges)(priority)

      while (priorityQueue.nonEmpty && currentNumberOfEdges < finalNumberOfEdges && disjointSet.numberOfComponents > 1)
        val weightedEdge@WeightedEdge(vertex1, vertex2, weight) = priorityQueue.dequeue()
        if (disjointSet.union(vertex1, vertex2))
          minSpanningTree.addVertex(vertex1)
          minSpanningTree.addVertex(vertex2)
          minSpanningTree.addEdge(weightedEdge)
          currentNumberOfEdges += 1
  }