package data.structures.mutable.graph.cycle

import data.structures.mutable.graph.Graph

import scala.collection.mutable

object CycleDetector {
  /**
   * Constructs an object for checking whether a graph is acyclic or not.
   *
   * @param graph graph to check.
   * @tparam V type of vertices in graph.
   * @tparam E type of edges in graph.
   * @return an object for checking whether a graph is acyclic or not.
   */
  def apply[V, E[_]](graph: Graph[V, E]): CycleDetector[V, E] = new CycleDetector(graph)
}

/**
 * Class for checking whether a graph is acyclic or not.
 *
 * @param graph graph to check.
 * @tparam V type of vertices in graph.
 * @tparam E type of edges in graph.
 * @author Pepe Gallardo.
 */
class CycleDetector[V, +E[_]](graph: Graph[V, E]) {
  private val visited = mutable.Set[V]()
  private var acyclic = true
  run()

  /**
   * Returns `true` if graph has no cycles.
   *
   * @return `true` if graph has no cycles.
   */
  def isAcyclic: Boolean =
    acyclic

  /** Returns `true` if graph has any cycle.
   *
   * @return `true` if graph has any cycle.
   */
  def isCyclic: Boolean = !acyclic

  private def depthFirst(previous: V, current: V): Unit = {
    visited.add(current)
    val iterator = graph.successors(current).iterator
    while (acyclic && iterator.hasNext) {
      val successor = iterator.next()
      if (!visited(successor)) {
        depthFirst(current, successor)
      } else if (successor != previous) {
        acyclic = false
      }
    }
  }

  private def run(): Unit = {
    val iterator = graph.vertices.iterator
    while (acyclic && iterator.hasNext) {
      val vertex = iterator.next()
      if (!visited(vertex)) {
        depthFirst(vertex, vertex)
      }
    }
  }
}
