package data.structures.mutable.heap

import scala.reflect.ClassTag

object MinHeap {
  private inline val rootIndex = 0
  private[heap] inline val defaultCapacity = 128

  /**
   * Constructs a new `MinHeap`.
   *
   * @param initialCapacity initial capacity of heap. Will be expanded as needed.
   * @param priority        `Ordering` describing priorities of elements stored in heap.
   * @param classTagT       a class tag for type of elements stored in heap.
   * @tparam T Type of elements stored in heap.
   * @return a new `MinHeap`.
   */
  def apply[T](initialCapacity: Int)(using priority: Ordering[T])(using classTagT: ClassTag[T])
  : MinHeap[T] =
    new MinHeap(initialCapacity)(using priority)(using classTagT)

  /**
   * Constructs a new `MinHeap` using default capacity (will be expanded as needed).
   *
   * @param priority  `Ordering` describing priorities of elements stored in heap.
   * @param classTagT a class tag for type of elements stored in heap.
   * @tparam T Type of elements stored in heap.
   * @return a new `MinHeap`.
   */
  def apply[T](using priority: Ordering[T])(using classTagT: ClassTag[T]): MinHeap[T] =
    new MinHeap(defaultCapacity)(using priority)(using classTagT)
}

/**
 * This class implements a min-heap for a set of elements, so that each element may only be include once in the heap.
 * In order to test for equality of elements, methods `equal` and `hashCode` are used. Each element has a priority
 * (defined by `priority` parameter). This priority is used to sort elements inside the heap so that the smaller the
 * element the sooner it will be extracted from the heap. Once an element has been inserted, its priority can be
 * updated (by doing an insertion of an equal element with a different priority) and its position within the heap
 * will be adjusted accordingly.
 * In order to locate an element in the heap (for updating its priority) a `Locator` can be used if several consecutive
 * operations are going to be performed. This will speedup those operations.
 *
 * @param initialCapacity initial capacity of heap. Will be expanded as needed.
 * @param priority        `Ordering` describing priorities of elements stored in heap.
 * @param classTagT       a class tag for type of elements stored in heap.
 * @tparam T Type of elements stored in heap.
 * @author Pepe Gallardo
 */
class MinHeap[T](initialCapacity: Int)(using priority: Ordering[T])(using classTagT: ClassTag[T]) {
  private def check(): Unit = {
    for (i <- 0 until sz) {
      assert(heapElements(i) == hashTable.keys(hashTableIndexes(i)),
        s"${heapElements(i)} ${hashTable.keys(hashTableIndexes(i))}")
    }

    for (i <- hashTable.heapIndexes.indices) {
      if (hashTable.heapIndexes(i) >= 0) {
        assert(heapElements(hashTable.heapIndexes(i)) == hashTable.keys(i),
          s"${hashTable.heapIndexes(i)}  ${heapElements(hashTable.heapIndexes(i))} ${hashTable.keys(i)}")
      }
    }
  }

  // elements in complete binary heap
  private var heapElements = new Array[T](initialCapacity)

  // indexes in hash table for each element in heap
  private[heap] var hashTableIndexes = new Array[Int](initialCapacity)
  private var sz = 0

  // the hash table for this heap
  protected val hashTable = new HashTableHeapIndexes[T](initialCapacity * 2, this)

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
    index > MinHeap.rootIndex

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
  def locatorFor(element: T): Locator =
    new Locator(hashTable.findOrReserve(element))

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
   * Inserts an element in the heap using the provided locator of the element. If the element was already in the heap
   * this operation can be used to update its priority within the heap (the element should be `equal` to previous one
   * but its `priority` can be different). Operation is O(log n).
   *
   * @param locator locator for accessing the element.
   * @param element element to insert in heap.
   */
  def insert(locator: Locator, element: T): Unit =
    ensureCapacity()
    val hashTableIndex = locator.index
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
      heapElements(heapIndex) = element
      heapifyUpFrom(heapIndex)
      heapifyDownFrom(heapIndex)
    }

  /**
   * Inserts an element in the heap returning a locator that can later be used for fast access to the element both in
   * the heap or in the map. If the element was already in the heap this operation can be used to update its priority
   * within the heap (the element should be `equal` to previous one but its `priority` can be different). Operation
   * is O(log n).
   *
   * @param element element to insert in heap.
   * @return a locator that can later be used for fast access to the element.
   *
   */
  def insert(element: T): Locator = {
    ensureCapacity()
    val heapIndex = sz
    val (inserted, hashTableIndex) = hashTable.insert(element, heapIndex)

    //todo hashTable.insert may lead to rehashing and then hashTableIndex is wrong
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
      heapElements(heapIndex) = element
      heapifyUpFrom(heapIndex)
      heapifyDownFrom(heapIndex)
    }
    new Locator(hashTableIndex)
  }

  /**
   * Increases an element that is already in the heap using the provided index of the element in the hash table. Does
   * nothing if element is not in heap or if new value does not increase stored one.
   *
   * @param hashTableIndex index of the element in the hash table.
   * @param element        element to increase in heap.
   */
  private def increaseIndex(hashTableIndex: Int, element: T): Boolean = {
    var increased = false
    if (hashTable.isInHeap(hashTableIndex)) {
      // locate already inserted element and update its priority
      val heapIndex = hashTable.heapIndexes(hashTableIndex)
      require(heapElements(heapIndex) == element, s"${heapElements(heapIndex)} $element")

      val cmp = priority.compare(element, heapElements(heapIndex))
      if (cmp > 0) {
        heapElements(heapIndex) = element
        heapifyDownFrom(heapIndex)
        increased = true
      }
    }
    increased
  }

  /**
   * Increases an element that is already in the heap using the provided locator of the element. Does nothing if element
   * is not in heap or if new value does not increase stored one. Operation is O(log n).
   *
   * @param locator locator for accessing the element.
   * @param element element to increase in heap.
   */
  def increase(locator: Locator, element: T): Boolean =
    increaseIndex(locator.index, element)

  /**
   * Increases an element that is already in the heap. Does nothing if element is not in heap or if new value does not
   * increase stored one. Operation is O(log n).
   *
   * @param element element to increase in heap.
   * @return a locator that can later be used for fast access to the element.
   *
   */
  def increase(element: T): (Boolean, Locator) = {
    val hashTableIndex = hashTableIndexFor(element)
    val increased = increaseIndex(hashTableIndex, element)
    (increased, new Locator(hashTableIndex))
  }

  /**
   * Decreases an element that is already in the heap using the provided index of the element in the hash table. Does
   * nothing if element is not in heap or if new value does not decrease stored one.
   *
   * @param hashTableIndex index of the element in the hash table.
   * @param element        element to increase in heap.
   */
  private def decreaseIndex(hashTableIndex: Int, element: T): Boolean = {
    var decreased = false
    if (hashTable.isInHeap(hashTableIndex)) {
      // locate already inserted element and update its priority
      val heapIndex = hashTable.heapIndexes(hashTableIndex)
      require(heapElements(heapIndex) == element, s"${heapElements(heapIndex)} $element")

      val cmp = priority.compare(element, heapElements(heapIndex))
      if (cmp < 0) {
        heapElements(heapIndex) = element
        heapifyUpFrom(heapIndex)
        decreased = true
      }
    }
    decreased
  }

  /**
   * Decreases an element that is already in the heap using the provided locator of the element. Does nothing if element
   * is not in heap or if new value does not decrease stored one. Operation is O(log n).
   *
   * @param locator locator for accessing the element.
   * @param element element to decrease in heap.
   */
  def decrease(locator: Locator, element: T): Boolean =
    decreaseIndex(locator.index, element)

  /**
   * Decreases an element that is already in the heap. Does nothing if element is not in heap or if new value does not
   * decrease stored one. Operation is O(log n).
   *
   * @param element element to decrease in heap.
   * @return a locator that can later be used for fast access to the element.
   *
   */
  def decrease(element: T): (Boolean, Locator) = {
    val hashTableIndex = hashTableIndexFor(element)
    val decreased = decreaseIndex(hashTableIndex, element)
    (decreased, new Locator(hashTableIndex))
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
    heapElements(MinHeap.rootIndex)
  }

  /**
   * Returns a locator for accessing first element.
   * @return a locator for accessing first element.
   */
  def locatorForFirst: Locator = {
    if (sz < 1) {
      throw new NoSuchElementException("locatorForFirst: heap is empty")
    }
    Locator(hashTableIndexes(MinHeap.rootIndex))
  }

  /**
   * Returns the first element in the heap (the one with the smallest priority) and also removes such element from
   * heap. Operation is O(log n).
   *
   * @return the first element in the heap (the one with the smallest priority).
   */
  def extractFirst(): T = {
    if (sz < 1) {
      throw new NoSuchElementException("extractFirst: heap is empty")
    }
    val first = heapElements(MinHeap.rootIndex)
    hashTable.heapIndexes(hashTableIndexes(MinHeap.rootIndex)) = HashTableHeapIndexes.reservedMark

    sz -= 1
    if (sz > 0) {
      heapElements(MinHeap.rootIndex) = heapElements(sz)
      hashTableIndexes(MinHeap.rootIndex) = hashTableIndexes(sz)
      hashTable.heapIndexes(hashTableIndexes(sz)) = MinHeap.rootIndex
      heapifyDownFrom(MinHeap.rootIndex)
    }
    heapElements(sz) = null.asInstanceOf[T] // let GC reclaim memory
    hashTableIndexes(sz) = HashTableHeapIndexes.freeMark
    first
  }

  private def containsIndex(hashTableIndex: Int): Boolean =
    hashTable.isInHeap(hashTableIndex)

  /**
   * Checks whether an element is currently included in heap. Operation is O(1) effective.
   *
   * @param element element to check for inclusion.
   * @return `true` if element is currently included in heap.
   */
  def contains(element: T): Boolean = {
    val hashTableIndex = hashTableIndexFor(element)
    containsIndex(hashTableIndex)
  }

  /**
   * Checks whether an element is currently included in heap. Operation is O(1).
   *
   * @param locator locator referencing element to check for inclusion.
   * @return `true` if element is currently included in heap.
   */
  def contains(locator: Locator): Boolean =
    containsIndex(locator.index)
}

