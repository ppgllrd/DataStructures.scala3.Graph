package graph.traversal

import graph.DirectedEdge

import scala.collection.Set

/**
 * An interface for data structures that can be traversed (must provide `successors` method).
 *
 * @tparam V type of elements in data structure.
 * @author Pepe Gallardo
 */
trait Traversable[V]:
  def successors(vertex: V): Set[V]

  def depthFirstTraversal(source: V): Traversal[V] =
    val dft = new DepthFirstTraversal(source, this)
    dft.traverse()
    dft

  def breadthFirstTraversal(source: V): Traversal[V] =
    val bft = new BreadthFirstTraversal(source, this)
    bft.traverse()
    bft

  def withContainerTraversal(_source: V, _container: Container[DirectedEdge[V]]): Traversal[V] =
    val _traversable = this
    val traversal = new Traversal[V]:
      protected val source: V = _source
      protected val container: Container[DirectedEdge[V]] = _container
      protected val traversable: Traversable[V] = _traversable
    traversal.traverse()
    traversal
