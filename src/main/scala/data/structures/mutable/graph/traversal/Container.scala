package data.structures.mutable.graph.traversal

import scala.collection.mutable

/**
 * A container that is used to store yet to be visited vertices during a traversal.
 *
 * @tparam T type of elements to store.
 * @author Pepe Gallardo
 */
trait Container[T]:
  /**
   * Checks if container is empty.
   *
   * @return `true` if container is empty, `false` otherwise.
   */
  def isEmpty: Boolean

  /**
   * Checks if container is non empty.
   *
   * @return `true` if container is non empty, `false` otherwise.
   */
  def nonEmpty: Boolean = !isEmpty

  /**
   * Adds an element to the container.
   *
   * @param element element to add to container.
   */
  def add(element: T): Unit

  /**
   * Extracts and returns first element from container.
   *
   * @return first element from container.
   */
  def remove(): T

/**
 * A LIFO container (used to implement depth first traversal).
 *
 * @tparam T type of elements to store.
 */
class StackContainer[T] extends Container[T]:
  private val stack = mutable.Stack[T]()

  override def isEmpty: Boolean = stack.isEmpty

  override def add(element: T): Unit = stack.push(element)

  override def remove(): T = stack.pop()

/**
 * A FIFO container (used to implement breadth first traversal).
 *
 * @tparam T type of elements to store.
 */
class QueueContainer[T] extends Container[T]:
  private val queue = mutable.Queue[T]()

  override def isEmpty: Boolean = queue.isEmpty

  override def add(element: T): Unit = queue.enqueue(element)

  override def remove(): T = queue.dequeue()