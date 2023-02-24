package data.structures.mutable.graph.traversal

import data.structures.mutable.graph.*

import scala.collection.Set

/**
 * An interface for data structures that can be traversed (must provide `successors` method).
 *
 * @tparam V type of elements in data structure.
 * @author Pepe Gallardo
 */
trait Traversable[V]:
  def successors(vertex: V): Set[V]

  /**
   * Returns a depth first traversal starting from element `source`.
   *
   * @param source element where traversal should start.
   * @return a depth first traversal starting from element `source`.
   */
  def depthFirstTraversal(source: V): Traversal[V] =
    val dft = new DepthFirstTraversal(source, this)
    dft.traverse()
    dft

  /**
   * Returns a breadth first traversal starting from element `source`.
   *
   * @param source element where traversal should start.
   * @return a breadth first traversal starting from element `source`.
   */
  def breadthFirstTraversal(source: V): Traversal[V] =
    val bft = new BreadthFirstTraversal(source, this)
    bft.traverse()
    bft

  /**
   * Returns a traversal starting from element `_source`. The order in which elements are traversed depends on the
   * policy implemented by the container passed as parameter.
   *
   * @param _source    element where traversal should start.
   * @param _container container whose policy defines order in which elements are traversed.
   * @return a traversal starting from element `_source`.
   */
  def withContainerTraversal(_source: V, _container: Container[DirectedEdge[V]]): Traversal[V] =
    val _traversable = this
    val traversal = new Traversal[V]:
      protected val source: V = _source
      protected val container: Container[DirectedEdge[V]] = _container
      protected val traversable: Traversable[V] = _traversable
    traversal.traverse()
    traversal
