package data.structures.mutable.graph.connectivity

import data.structures.mutable.disjointSet.DisjointSet
import data.structures.mutable.disjointSet.indexedSet.ArrayIndexedSet
import data.structures.mutable.graph.*

import scala.reflect.ClassTag

object ConnectivityDisjointSet {
  /**
   * Constructs an object for checking whether an undirected graph is connected or not.
   *
   * @param undirectedGraph the undirected graph to check.
   * @tparam V type of vertices in graph.
   * @return an object for checking whether a graph is connected or not.
   */
  def apply[V](undirectedGraph: UndirectedGraph[V])(using classTagV: ClassTag[V]): ConnectivityDisjointSet[V] =
    new ConnectivityDisjointSet(undirectedGraph)
}

/**
 * Class for checking whether an undirected graph is connected or not.
 *
 * @param undirectedGraph the undirected graph to check.
 * @tparam V type of vertices in graph.
 * @author Pepe Gallardo.
 */
class ConnectivityDisjointSet[V](undirectedGraph: UndirectedGraph[V])(using classTagV: ClassTag[V]) {
  private var connected = true
  run()

  /**
   * Returns `true` if graph is connected.
   *
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
    val vertices = undirectedGraph.vertices
    val indexedSet = ArrayIndexedSet(vertices.toArray)
    val disjointSet = DisjointSet.fromIndexedSet(indexedSet)

    val iterator = undirectedGraph.edges.iterator
    while (disjointSet.numberOfComponents > 1 && iterator.hasNext) {
      val Edge(vertex1, vertex2) = iterator.next()
      disjointSet.union(vertex1, vertex2)
    }

    connected = disjointSet.numberOfComponents == 1
  }
}
