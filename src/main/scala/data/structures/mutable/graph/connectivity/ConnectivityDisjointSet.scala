package data.structures.mutable.graph.connectivity

import data.structures.mutable.disjointSet.DisjointSet
import data.structures.mutable.disjointSet.indexedSet.ArrayIndexedSet
import data.structures.mutable.graph.{Edge, Graph, WeightedEdge, WeightedGraph}

import scala.reflect.ClassTag

object ConnectivityDisjointSet {
  /**
   * Constructs an object for checking whether a graph is connected or not.
   * @param graph the graph to check.
   * @tparam V type of vertices in graph.
   * @return an object for checking whether a graph is connected or not.
   */
  def apply[V](graph: Graph[V, Edge])(using classTagV: ClassTag[V]): ConnectivityDisjointSet[V] =
    new ConnectivityDisjointSet(graph)
}

/**Connectivity
 * Class for checking whether a graph is connected or not.
 * @param graph the graph to check.
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo.
 */
class ConnectivityDisjointSet[V](graph: Graph[V, Edge])(using classTagV: ClassTag[V]) {
  private var connected = true
  run()

  /**
   * Returns `true` if graph is connected.
   * @return `true` if graph is connected.
   */
  def isConnected: Boolean = connected

  /**
   * Returns `true` if graph is not connected.
   *
   * @return `true` if graph is not connected.
   */
  def isDisconnected: Boolean = !connected

  private def run(): Unit = {

    val vertices = graph.vertices
    val indexedSet = ArrayIndexedSet(vertices.toArray)
    val disjointSet = DisjointSet.fromIndexedSet(indexedSet)

    val iterator = graph.edges.iterator
    while (disjointSet.numberOfComponents > 1 && iterator.hasNext) {
      val edge: Edge[V] = iterator.next()
      disjointSet.union(edge.vertex1, edge.vertex2)
    }

    connected = disjointSet.numberOfComponents == 1
  }
}
