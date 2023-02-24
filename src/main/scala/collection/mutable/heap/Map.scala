package collection.mutable.heap

/**
 * A trait representing operations that can be performed with map in a MinHeapMap.
 *
 * @tparam T Type of elements stored in heap.
 * @tparam V Type of values associated in map. 
 * @author Pepe Gallardo
 */
trait Map[T, V] {
  /**
   * Inserts or updates the value associated with the element in the map. Operation is O(1) effective.
   *
   * @param element element acting as key in map.
   * @param value   value associated to key in map.
   * @return a locator that can later be used for fast access to the element both in the heap or in the map.
   */
  def update(element: T, value: V): Locator

  /**
   * Inserts or updates the value associated with the element corresponding to provided locator in the map. Operation is O(1).
   *
   * @param locator locator of element acting as key in map.
   * @param value   value associated to key in map.
   */
  def update(locator: Locator, value: V): Unit

  /**
   * Returns value associated with provided element in map. Operation is O(1) effective.
   *
   * @param element element acting as key in map.
   * @return value associated with provided element in map. Raises exception if element is not in map.
   */
  def apply(element: T): V

  /**
   * Returns value associated with element corresponding to provided locator in map. Operation is O(1).
   *
   * @param locator locator of element acting as key in map.
   * @return value associated with element corresponding to provided locator in map. Raises exception if element is
   *         not in map.
   */
  def apply(locator: Locator): V

  /**
   * Returns `Some(value)` where value is the one associated with provided element in map or `None` if element is
   * not in map. Operation is O(1) effective.
   *
   * @param element element acting as key in map.
   * @return `Some(value)` where value is the one associated with provided element in map or `None` if element is
   *         not in map.
   */
  def get(element: T): Option[V]

  /**
   * Returns `Some(value)` where value is the one associated with element corresponding to provided locator in map
   * or `None` if element is not in map. Operation is O(1).
   *
   * @param locator locator of element acting as key in map.
   * @return `Some(value)` where value is the one associated with element corresponding to provided locator in map
   *         or `None` if element is not in map.
   */
  def get(locator: Locator): Option[V]
}
