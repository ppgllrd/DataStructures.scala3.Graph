package data.structures.mutable.graph.connectivity

import data.structures.mutable.graph.Graph

object Connectivity {
  /**
   * Constructs an object for checking whether a graph is connected or not.
   *
   * @param graph the graph to check.
   * @tparam V type of vertices in graph.
   * @tparam E type of edges in graph.
   * @return an object for checking whether a graph is connected or not.
   */
  def apply[V, E[_]](graph: Graph[V, E]): Connectivity[V, E] = new Connectivity(graph)
}

/**
 * Class for checking whether a graph is connected or not.
 *
 * @param graph the graph to check.
 * @tparam V type of vertices in graph.
 * @tparam E type of edges in graph.
 * @author Pepe Gallardo.
 */
class Connectivity[V, +E[_]](graph: Graph[V, E]) {
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
    val vertices = graph.vertices
    if (vertices.nonEmpty) {
      val source = vertices.head
      val dft = graph.depthFirstTraversal(source)
      val iterator = vertices.iterator
      while (connected && iterator.hasNext) {
        val vertex = iterator.next()
        if (!dft.isReachable(vertex))
          connected = false
      }
    }
  }
}
