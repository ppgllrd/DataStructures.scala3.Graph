package data.structures.mutable.graph.traversal

import data.structures.mutable.graph
import data.structures.mutable.graph.{DirectedEdge, traversal}

import scala.collection.*

/**
 * Interface representing a traversal of a data structure.
 *
 * @tparam V type of elements traversed.
 * @author Pepe Gallardo
 */
trait Traversal[V]:
  /**
   * Source of traversal.
   */
  protected val source: V
  /**
   * Data structure that should be traversed.
   */
  protected val traversable: traversal.Traversable[V]
  /**
   * Container used for storing yet to be visited elements. 
   */
  protected val container: Container[DirectedEdge[V]]

  /**
   * Stores previous element in traversal for each visited element.
   */
  private val sourceOf: mutable.Map[V, V] = mutable.Map[V, V]()
  /**
   * `true` if traversal has already been completed.
   */
  private var traversed = false

  /**
   * Performs a traversal of the data structure.
   */
  def traverse(): Unit =
    container.add(DirectedEdge(source, source))
    while container.nonEmpty do
      val DirectedEdge(src, dst) = container.remove()
      if !visited(dst) then
        sourceOf(dst) = src
        for v <- traversable.successors(dst) do
          if !sourceOf.contains(v) then
            container.add(DirectedEdge(dst, v))
    traversed = true

  private inline def visited(inline v: V): Boolean =
    sourceOf.contains(v)

  /**
   * Builds a path from source of traversal to `destination` vertex.
   *
   * @param destination destination vertex to reach.
   * @return a path from source of traversal to `destination` vertex
   */
  def pathTo(destination: V): List[V] =
    require(traversed, "Traversal must be traversed")
    require(isReachable(destination), s"No path to $destination from $source")
    var path = List[V](destination)
    while path.head != source do
      path ::= sourceOf(path.head)
    path

  /**
   * Checks whether a vertex can be reached during the traversal.
   *
   * @param destination destination vertex to reach.
   * @return `true` if vertex `destination` can be reached during the traversal.
   */
  def isReachable(destination: V): Boolean =
    require(traversed, "Traversal must be traversed")
    sourceOf.isDefinedAt(destination)

/**
 * Class representing a depth first traversal.
 *
 * @param source      source vertex for starting the traversal.
 * @param traversable data structure that should be traversed.
 * @tparam V type of elements to be traversed.
 */
class DepthFirstTraversal[V](val source: V, val traversable: traversal.Traversable[V]) extends Traversal[V]:
  protected val container = new StackContainer[DirectedEdge[V]]()

/**
 * Class representing a breadth first traversal.
 *
 * @param source      source vertex for starting the traversal.
 * @param traversable data structure that should be traversed.
 * @tparam V type of elements to be traversed.
 */
class BreadthFirstTraversal[V](val source: V, val traversable: traversal.Traversable[V]) extends Traversal[V]:
  protected val container = new QueueContainer[DirectedEdge[V]]()