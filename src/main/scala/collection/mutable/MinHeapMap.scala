package collection.mutable

import scala.reflect.ClassTag

/**
 * A locator can be used for fast access to elements stored in heap or map.
 */
class Locator private[mutable](val index: Int) extends AnyVal

object MinHeapMap {
  private inline val freeMark = -1
  private inline val reservedMark = -2

  private inline val maximumLoadFactor = 0.5
  private inline val rootIndex = 0
  private inline val defaultCapacity = 128

  /**
   * Constructs a new `MinHeapMap`.
   *
   * @param initialCapacity initial capacity of heap. Will be expanded as needed.
   * @param priority        `Ordering` describing priorities of elements stored in heap.
   * @param classTagT       a class tag for type of elements stored in heap.
   * @param classTagV       a class tag for type of values associated in map.
   * @tparam T Type of elements stored in heap.
   * @tparam V Type of values associated in map.
   * @return a new `MinHeapMap`.
   */
  def apply[T, V](initialCapacity: Int)(using priority: Ordering[T])(using classTagT: ClassTag[T])
    (using classTagV: ClassTag[V]): MinHeapMap[T, V] =
    new MinHeapMap(initialCapacity)(using priority)(using classTagT)(using classTagV)

  /**
   * Constructs a new `MinHeapMap` using default capacity (Will be expanded as needed).
   *
   * @param priority  `Ordering` describing priorities of elements stored in heap.
   * @param classTagT a class tag for type of elements stored in heap.
   * @param classTagV a class tag for type of values associated in map.
   * @tparam T Type of elements stored in heap.
   * @tparam V Type of values associated in map.
   * @return a new `MinHeapMap`.
   */
  def apply[T, V](using priority: Ordering[T])(using classTagT: ClassTag[T])
    (using classTagV: ClassTag[V]): MinHeapMap[T, V] =
    new MinHeapMap(defaultCapacity)(using priority)(using classTagT)(using classTagV)
}

/**
 * This class implements a min-heap for a set of elements, so that each element may only be include once in the heap.
 * In order to test for equality of elements, methods `equal` and `hashCode` are used. Each element has a priority
 * (defined by `priority` parameter). This priority is used to sort elements inside the heap so that the smaller the
 * element the sooner it will be extracted from the heap. Once an element has been inserted, its priority can be
 * updated (by doing an insertion of an equal element with a different priority) and its position within the heap
 * will be adjusted accordingly.
 * Additionally this class also provides a map or dictionary from elements stored in the heap (acting as keys) to
 * values associated with them. This map is implemented by using a linear probing hash table which provides fast
 * insertions and lookups.
 * In order to locate an element in the heap (for updating its priority or for setting/retrieving its associated
 * value, a `Locator` can be used if several consecutive operations are going to be performed. This will speedup
 * those operations.
 *
 * @param initialCapacity initial capacity of heap. Will be expanded as needed.
 * @param priority        `Ordering` describing priorities of elements stored in heap.
 * @param classTagT       a class tag for type of elements stored in heap.
 * @param classTagV       a class tag for type of values associated in map.
 * @tparam T Type of elements stored in heap.
 * @tparam V Type of values associated in map.
 * @author Pepe Gallardo
 */
class MinHeapMap[T, V](initialCapacity: Int)(using priority: Ordering[T])
  (using classTagT: ClassTag[T])(using classTagV: ClassTag[V]) {

  /**
   * This hash table stores elements in heap as keys, their index within the heap (for fast lookup of such elements)
   * and associated values in map.
   */
  private class HashTable {
    // elements in heap which are also keys in dictionary
    var keys = new Array[T](initialCapacity * 2)
    // values associated to each key in dictionary
    var values = new Array[V](initialCapacity * 2)
    // indexes for locating each element in heap
    var heapIndexes: Array[Int] = Array.fill[Int](initialCapacity * 2)(MinHeapMap.freeMark)

    // initially all cells are free
    inline def isFree(inline index: Int): Boolean =
      heapIndexes(index) == MinHeapMap.freeMark

    // a cell is reserved if it stores an element that was extracted from heap or if it is reserved for inserting an
    // element that has already an associated locator
    inline def isReserved(inline index: Int): Boolean =
      heapIndexes(index) == MinHeapMap.reservedMark

    // a cell is ocupied if it corresponds to an element in heap (heapIndexes > 0) or if it is reserved
    inline def isOccupied(inline index: Int): Boolean =
      !isFree(index)

    // number of cells which are occupied in hash table
    private var sz = 0

    // computes hash of an element
    private def hash(key: T): Int = (key.hashCode() & 0x7fffffff) % keys.length

    // searches for index of an element in the hash table
    def indexOf(key: T): Int = {
      var index = hash(key)
      while (isOccupied(index) && keys(index) != key) {
        index = (index + 1) % keys.length
      }
      index
    }

    // computes current load factor of hash table
    private def loadFactor: Double = sz.toDouble / keys.length

    // performs rehashing if current load factor exceeds maximum allowed one
    private def rehashing(): Unit = {
      if (loadFactor > MinHeapMap.maximumLoadFactor) {
        val oldKeys = keys
        val oldValues = values
        val oldHeapIndexes = heapIndexes
        keys = new Array[T](keys.length * 2)
        values = new Array[V](values.length * 2)
        heapIndexes = Array.fill[Int](heapIndexes.length * 2)(MinHeapMap.freeMark)
        for (i <- oldKeys.indices) {
          if (isOccupied(i)) {
            val index = indexOf(oldKeys(i))
            keys(index) = oldKeys(i)
            values(index) = oldValues(i)
            heapIndexes(index) = oldHeapIndexes(i)
          }
        }
      }
    }

    /**
     * Inserts an element and its associated index in heap in hash table. Operation is O(log n).
     *
     * @param index     index in hash table where element should be stored (according to its hash code).
     * @param key       element to insert in hash table.
     * @param heapIndex index in heap where this element is stored.
     * @return `true` if element was inserted or `false` if element was already in hash table.
     */
    def insert(index: Int, key: T, heapIndex: Int): Boolean = {
      rehashing()

      var inserted = false
      if (isFree(index) || isReserved(index)) {
        keys(index) = key
        heapIndexes(index) = heapIndex
        inserted = true
        sz += 1
      }
      inserted
    }

    def insert(key: T, heapIndex: Int): (Int, Boolean) = {
      val index = indexOf(key)
      val inserted = insert(index, key, heapIndex)
      (index, inserted)
    }
  }

  private def check(): Unit = {
    for (i <- 0 until sz) {
      assert(heapElements(i) == hashTable.keys(hashTableIndexes(i)),
        s"${heapElements(i)} ${hashTable.keys(hashTableIndexes(i))}")
    }
  }

  // the hash table for this heap
  private val hashTable = new HashTable

  // elements in complete binary heap
  private var heapElements = new Array[T](initialCapacity)

  // indexes in hash table for each element in heap
  private var hashTableIndexes = new Array[Int](initialCapacity)
  private var sz = 0

  // returns index of parent node in heap
  private inline def parentIndexOf(inline index: Int): Int =
    (index - 1) / 2

  // returns index of left child node in heap
  private inline def leftChildIndexOf(inline index: Int): Int =
    index * 2 + 1

  // checks if an index corresponds to an element in heap
  private inline def isValidIndex(inline index: Int): Boolean =
    index < sz

  // checks if an element is not the root of heap
  private inline def hasParentIndex(inline index: Int): Boolean =
    index > MinHeapMap.rootIndex

  // moves element in heap at given index up as needed in order to restore heap order property
  private def heapifyUpFrom(index: Int): Unit = {
    var elementIndex = index
    val element = heapElements(elementIndex)
    val elementHashTableIndex = hashTableIndexes(elementIndex)

    var sorted = false
    var moved = false
    while (!sorted && hasParentIndex(elementIndex)) {
      val parentIndex = parentIndexOf(elementIndex)
      val parent = heapElements(parentIndex)
      if (priority.compare(element, parent) >= 0) {
        sorted = true
      } else {
        // move parent down
        val parentHashTableIndex = hashTableIndexes(parentIndex)

        heapElements(elementIndex) = parent
        hashTableIndexes(elementIndex) = parentHashTableIndex

        hashTable.heapIndexes(parentHashTableIndex) = elementIndex

        moved = true
        elementIndex = parentIndex
      }
    }
    if (moved) {
      // place initial element in its right position
      heapElements(elementIndex) = element
      hashTableIndexes(elementIndex) = elementHashTableIndex

      hashTable.heapIndexes(elementHashTableIndex) = elementIndex
    }
  }

  // moves element in heap at given index down as needed in order to restore heap order property
  private def heapifyDownFrom(index: Int): Unit = {
    var elementIndex = index
    val element = heapElements(elementIndex)
    val elementHashTableIndex = hashTableIndexes(elementIndex)

    var sorted = false
    var moved = false
    var minimumChildIndex = 0
    while ( {
      minimumChildIndex = leftChildIndexOf(elementIndex)
      !sorted && isValidIndex(minimumChildIndex)
    }) {
      // find minimum child
      val rightChildIndex = minimumChildIndex + 1
      if (isValidIndex(rightChildIndex) &&
        priority.compare(heapElements(rightChildIndex), heapElements(minimumChildIndex)) < 0) {
        minimumChildIndex = rightChildIndex
      }
      val minimumChild = heapElements(minimumChildIndex)
      if (priority.compare(minimumChild, element) >= 0) {
        sorted = true
      } else {
        // move minimum children up
        val minimumChildHashTableIndex = hashTableIndexes(minimumChildIndex)

        heapElements(elementIndex) = minimumChild
        hashTableIndexes(elementIndex) = minimumChildHashTableIndex

        hashTable.heapIndexes(minimumChildHashTableIndex) = elementIndex

        moved = true
        elementIndex = minimumChildIndex
      }
    }
    if (moved) {
      // place initial element in its right position
      heapElements(elementIndex) = element
      hashTableIndexes(elementIndex) = elementHashTableIndex

      hashTable.heapIndexes(elementHashTableIndex) = elementIndex
    }
  }

  /**
   * Checks if heap stores no elements.
   *
   * @return `true` if heap stores no elements.
   */
  def isEmpty: Boolean = sz <= 0

  /**
   * Checks if heap stores at least one element.
   *
   * @return `true` if heap stores at least one element.
   */
  def nonEmpty: Boolean = sz > 0

  /**
   * Returns index in hash table corresponding to provided heap element.
   *
   * @param element a heap element.
   * @return index in hash table corresponding to provided heap element.
   */
  private def hashTableIndexFor(element: T): Int =
    hashTable.indexOf(element)

  /**
   * Returns a locator for provided heap element. A locator can later be used for fast access to the element both in
   * the heap and in the map. Operation is O(1) effective.
   *
   * @param element a heap element
   * @return a locator for provided heap element.
   */
  def locatorFor(element: T): Locator = {
    val hashTableIndex = hashTableIndexFor(element)
    if (hashTable.isFree(hashTableIndex)) {
      hashTable.keys(hashTableIndex) = element
      hashTable.heapIndexes(hashTableIndex) = MinHeapMap.reservedMark
    }
    new Locator(hashTableIndex)
  }

  /**
   * Expands arrays if we run out of capacity due to too much insertions.
   */
  private def ensureCapacity(): Unit = {
    if (sz >= heapElements.length) {
      // ensure capacity
      heapElements = Array.copyOf(heapElements, heapElements.length * 2)
      hashTableIndexes = Array.copyOf(hashTableIndexes, hashTableIndexes.length * 2)
    }
  }

  /**
   * Inserts an element in the heap using the provided index of the element in the hash table.
   *
   * @param hashTableIndex index of the element in the hash table.
   * @param element        element to insert in heap.
   */
  private def insertIndex(hashTableIndex: Int, element: T): Unit = {
    ensureCapacity()

    val heapIndex = sz
    val inserted = hashTable.insert(hashTableIndex, element, heapIndex)

    if (inserted) {
      // insert new element in heap
      heapElements(heapIndex) = element
      hashTableIndexes(heapIndex) = hashTableIndex
      heapifyUpFrom(heapIndex)
      sz += 1
    } else {
      // locate already inserted element and update its priority
      val heapIndex = hashTable.heapIndexes(hashTableIndex)
      require(heapElements(heapIndex) == element, s"${heapElements(heapIndex)} $element")
      val cmp = priority.compare(element, heapElements(heapIndex))

      if (cmp < 0) {
        heapElements(heapIndex) = element
        heapifyUpFrom(heapIndex)
      } else if (cmp > 0) {
        heapElements(heapIndex) = element
        heapifyDownFrom(heapIndex)
      }
    }
  }

  /**
   * Inserts an element in the heap using the provided locator of the element. If the element was already in the heap
   * this operation can be used to update its priority within the heap (the element should be `equal` to previous one
   * but its `priority` can be different). Operation is O(log n).
   *
   * @param locator locator for accessing the element.
   * @param element element to insert in heap.
   */
  def insert(locator: Locator, element: T): Unit =
    insertIndex(locator.index, element)

  /**
   * Inserts an element in the heap returning a locator that can later be used for fast access to the element both in
   * the heap or in the map. If the element was already in the heap this operation can be used to update its priority
   * within the heap (the element should be `equal` to previous one but its `priority` can be different). Operation
   * is O(log n).
   *
   * @param element element to insert in heap.
   * @return a locator that can later be used for fast access to the element both in the heap or in the map.
   *
   */
  def insert(element: T): Locator = {
    val hashTableIndex = hashTableIndexFor(element)
    insertIndex(hashTableIndex, element)
    new Locator(hashTableIndex)
  }

  /**
   * Returns the first element in the heap (the one with the smallest priority). Operation is O(1).
   *
   * @return the first element in the heap (the one with the smallest priority).
   */
  def first: T = {
    if (sz < 1) {
      throw new NoSuchElementException("first: heap is empty")
    }
    heapElements(MinHeapMap.rootIndex)
  }

  /**
   * Returns the first element in the heap (the one with the smallest priority) and also removes such element from
   * heap. Operation is O(log n).
   *
   * @return the first element in the heap (the one with the smallest priority).
   */
  def deleteFirst(): T = {
    if (sz < 1) {
      throw new NoSuchElementException("deleteFirst: heap is empty")
    }
    val first = heapElements(MinHeapMap.rootIndex)
    hashTable.heapIndexes(hashTableIndexes(MinHeapMap.rootIndex)) = MinHeapMap.reservedMark

    sz -= 1
    heapElements(0) = heapElements(sz)
    hashTableIndexes(0) = hashTableIndexes(sz)
    hashTable.heapIndexes(hashTableIndexes(sz)) = 0

    heapifyDownFrom(0)

    heapElements(sz) = null.asInstanceOf[T] // let GC reclaim memory

    first
  }

  /**
   * The map which is part of this `MinHeapMap`.
   */
  object map {
    private def updateIndex(hashTableIndex: Int, element: T, value: V): Locator = {
      if (hashTable.isFree(hashTableIndex)) {
        hashTable.keys(hashTableIndex) = element
        hashTable.heapIndexes(hashTableIndex) = MinHeapMap.reservedMark
      }
      hashTable.values(hashTableIndex) = value
      new Locator(hashTableIndex)
    }

    /**
     * Inserts or updates the value associated with the element in the map. Operation is O(1) effective.
     *
     * @param element element acting as key in map.
     * @param value   value associated to key in map.
     * @return a locator that can later be used for fast access to the element both in the heap or in the map.
     */
    def update(element: T, value: V): Locator = {
      val hashTableIndex = hashTable.indexOf(element)
      updateIndex(hashTableIndex, element, value)
    }

    /**
     * Inserts or updates the value associated with the element corresponding to provided locator in the map. Operation is O(1).
     *
     * @param locator locator of element acting as key in map.
     * @param value   value associated to key in map.
     */
    def update(locator: Locator, value: V): Unit = {
      val hashTableIndex = locator.index
      if (hashTable.isFree(hashTableIndex)) {
        hashTable.heapIndexes(hashTableIndex) = MinHeapMap.reservedMark
      }
      hashTable.values(hashTableIndex) = value
    }

    private def applyIndex(hashTableIndex: Int): V = {
      assert(!hashTable.isFree(hashTableIndex), "apply: element for locator is not in map")
      hashTable.values(hashTableIndex)
    }

    /**
     * Returns value associated with provided element in map. Operation is O(1) effective.
     *
     * @param element element acting as key in map.
     * @return value associated with provided element in map. Raises exception if element is not in map.
     */
    def apply(element: T): V = {
      val hashTableIndex = hashTable.indexOf(element)
      applyIndex(hashTableIndex)
    }

    /**
     * Returns value associated with element corresponding to provided locator in map. Operation is O(1).
     *
     * @param locator locator of element acting as key in map.
     * @return value associated with element corresponding to provided locator in map. Raises exception if element is
     *         not in map.
     */
    def apply(locator: Locator): V = {
      applyIndex(locator.index)
    }

    private def getIndex(hashTableIndex: Int): Option[V] = {
      if (hashTable.isFree(hashTableIndex)) {
        None
      } else if (hashTable.isReserved(hashTableIndex)) {
        val value = hashTable.values(hashTableIndex)
        Option(value)
      } else {
        Some(hashTable.values(hashTableIndex))
      }
    }

    /**
     * Returns `Some(value)` where value is the one associated with provided element in map or `None` if element is
     * not in map. Operation is O(1) effective.
     *
     * @param element element acting as key in map.
     * @return `Some(value)` where value is the one associated with provided element in map or `None` if element is
     *         not in map.
     */
    def get(element: T): Option[V] = {
      val hashTableIndex = hashTable.indexOf(element)
      getIndex(hashTableIndex)
    }

    /**
     * Returns `Some(value)` where value is the one associated with element corresponding to provided locator in map
     * or `None` if element is not in map. Operation is O(1).
     *
     * @param locator locator of element acting as key in map.
     * @return `Some(value)` where value is the one associated with element corresponding to provided locator in map
     *         or `None` if element is not in map.
     */
    def get(locator: Locator): Option[V] = {
      getIndex(locator.index)
    }
  }
}

