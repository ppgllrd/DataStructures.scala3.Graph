package data.structures.mutable.heap

import scala.annotation.targetName
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object IndexedHeap {
  private inline val rootIndex = 0
  private[heap] inline val defaultCapacity = 128

  /**
   * Constructs a new `IndexedHeap`.
   *
   * @param initialCapacity initial capacity of heap. Will be expanded as needed.
   * @param priority        `Ordering` describing priorities of elements stored in heap.
   * @param classTagT       a class tag for type of elements stored in heap.
   * @tparam T Type of elements stored in heap.
   * @return a new `IndexedHeap`.
   */
  def apply[T](initialCapacity: Int)(using priority: Ordering[T])(using classTagT: ClassTag[T])
  : IndexedHeap[T] =
    new IndexedHeap(initialCapacity)(using priority)(using classTagT)

  /**
   * Constructs a new `IndexedHeap` using default capacity (will be expanded as needed).
   *
   * @param priority  `Ordering` describing priorities of elements stored in heap.
   * @param classTagT a class tag for type of elements stored in heap.
   * @tparam T Type of elements stored in heap.
   * @return a new `IndexedHeap`.
   */
  def apply[T](using priority: Ordering[T])(using classTagT: ClassTag[T]): IndexedHeap[T] =
    new IndexedHeap(defaultCapacity)(using priority)(using classTagT)
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
class IndexedHeap[T](initialCapacity: Int)(using priority: Ordering[T])(using classTagT: ClassTag[T]) {
  private def check(): Unit = {
    /*
    for (i <- 0 until sz) {
      assert(heapElementsLocators(i) == hashTable.keys(hashTableIndexes(i)),
        s"${heapElementsLocators(i)} ${hashTable.keys(hashTableIndexes(i))}")
    }
    */

    for (i <- hashTable.heapIndexes.indices) {
      if (hashTable.heapIndexes(i) >= 0) {
        assert(heapElementsLocators(hashTable.heapIndexes(i)) == hashTable.keys(i),
          s"${hashTable.heapIndexes(i)}  ${heapElementsLocators(hashTable.heapIndexes(i))} ${hashTable.keys(i)}")
      }
    }
  }

  // locators for elements in complete binary heap
  private var heapElementsLocators = new Array[Int](initialCapacity)

  private var sz = 0

  // the hash table for this heap
  protected val hashTable = new HashTable[T](initialCapacity * 2)

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
    index > IndexedHeap.rootIndex

  private def keyFor(index: Int): T =
    hashTable.keyForLocator(heapElementsLocators(index))

  private def hashTableIndexFor(element: T): Int =
    hashTable.searchIndexOf(element)

  @targetName("hashTableIndexForLocator")
  protected def hashTableIndexFor(locator: Locator): Int =
    hashTable.locators(locator.index)

  private def hashTableIndexFor(index: Int): Int =
    hashTable.locators(heapElementsLocators(index))

  // moves element in heap at given index up as needed in order to restore heap order property
  private def heapifyUpFrom(index: Int): Unit = {
    var elementIndex = index
    val element = keyFor(elementIndex)
    val elementLocator = heapElementsLocators(elementIndex)
    val elementHashTableIndex = hashTableIndexFor(elementIndex)

    var sorted = false
    var moved = false
    while (!sorted && hasParentIndex(elementIndex)) {
      val parentIndex = parentIndexOf(elementIndex)
      val parent = keyFor(parentIndex)
      if (priority.compare(element, parent) >= 0) {
        sorted = true
      } else {
        // move parent down
        val parentHashTableIndex = hashTableIndexFor(parentIndex)
        heapElementsLocators(elementIndex) = heapElementsLocators(parentIndex)
        hashTable.heapIndexes(parentHashTableIndex) = elementIndex

        moved = true
        elementIndex = parentIndex
      }
    }
    if (moved) {
      // place initial element in its right position
      heapElementsLocators(elementIndex) = elementLocator
      hashTable.heapIndexes(elementHashTableIndex) = elementIndex
    }
  }

  // moves element in heap at given index down as needed in order to restore heap order property
  private def heapifyDownFrom(index: Int): Unit = {
    var elementIndex = index
    val element = keyFor(elementIndex)
    val elementLocator = heapElementsLocators(elementIndex)
    val elementHashTableIndex = hashTableIndexFor(elementIndex)

    var sorted = false
    var moved = false
    var minimumChildIndex = 0

    while ( {
      minimumChildIndex = leftChildIndexOf(elementIndex)
      !sorted && isValidIndex(minimumChildIndex)
    }) {
      var minimumChild = keyFor(minimumChildIndex)
      // find minimum child
      val rightChildIndex = minimumChildIndex + 1
      if (isValidIndex(rightChildIndex)) {
        val rightChild = keyFor(rightChildIndex)
        if(priority.compare(rightChild, minimumChild) < 0) {
          minimumChildIndex = rightChildIndex
          minimumChild = rightChild
        }
      }
      if (priority.compare(minimumChild, element) >= 0) {
        sorted = true
      } else {
        // move minimum children up
        val minimumChildHashTableIndex = hashTableIndexFor(minimumChildIndex)

        heapElementsLocators(elementIndex) = heapElementsLocators(minimumChildIndex)
        hashTable.heapIndexes(minimumChildHashTableIndex) = elementIndex

        moved = true
        elementIndex = minimumChildIndex
      }
    }
    if (moved) {
      // place initial element in its right position
      heapElementsLocators(elementIndex) = elementLocator
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
   * Returns a locator for provided heap element. A locator can later be used for fast access to the element both in
   * the heap and in the map. Operation is O(1) effective.
   *
   * @param element a heap element
   * @return a locator for provided heap element.
   */
  def locatorFor(element: T): Locator =
    hashTable.locatorFor(element)


  /**
   * Expands arrays if we run out of capacity due to too much insertions.
   */
  private def ensureCapacity(): Unit = {
    if (sz >= heapElementsLocators.length) {
      // ensure capacity
      heapElementsLocators = Array.copyOf(heapElementsLocators, heapElementsLocators.length * 2)
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
    val hashTableIndex = hashTableIndexFor(locator)
    val heapIndex = sz
    val (inserted, locatorIndex) = hashTable.insert(hashTableIndex, element, heapIndex)

    if (inserted) {
      // insert new element in heap
      heapElementsLocators(heapIndex) = locatorIndex
      heapifyUpFrom(heapIndex)
      sz += 1
    } else {
      // locate already inserted element and update its priority
      val heapIndex = hashTable.heapIndexes(hashTableIndex)
      require(keyFor(heapIndex) == element, s"${keyFor(heapIndex)} $element")
      heapElementsLocators(heapIndex) = locatorIndex
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
    val (inserted, locatorIndex, hashTableIndex) = hashTable.insert(element, heapIndex)

    if (inserted) {
      // insert new element in heap
      heapElementsLocators(heapIndex) = locatorIndex
      heapifyUpFrom(heapIndex)
      sz += 1
    } else {
      // locate already inserted element and update its priority
      val heapIndex = hashTable.heapIndexes(hashTableIndex)
      require(keyFor(heapIndex) == element, s"${keyFor(heapIndex)} $element")
      heapElementsLocators(heapIndex) = locatorIndex
      heapifyUpFrom(heapIndex)
      heapifyDownFrom(heapIndex)
    }
    hashTable.locatorFor(hashTableIndex)
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
      require(heapElementsLocators(heapIndex) == element, s"${heapElementsLocators(heapIndex)} $element")

      val cmp = priority.compare(element, keyFor(heapIndex))
      if (cmp > 0) {
        hashTable.keys(hashTableIndex) = element
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
    increaseIndex(hashTableIndexFor(locator), element)

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
    (increased, hashTable.locatorFor(hashTableIndex))
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
      require(keyFor(heapIndex) == element, s"${keyFor(hashTable.heapIndexes(hashTableIndex))} $element")

      val cmp = priority.compare(element, hashTable.keys(hashTableIndex))
      if (cmp < 0) {
        hashTable.keys(hashTableIndex) = element
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
    decreaseIndex(hashTableIndexFor(locator), element)

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
    (decreased, hashTable.locatorFor(hashTableIndex))
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
    keyFor(IndexedHeap.rootIndex)
  }

  /**
   * Returns a locator for accessing first element.
   * @return a locator for accessing first element.
   */
  def locatorForFirst: Locator = {
    if (sz < 1) {
      throw new NoSuchElementException("locatorForFirst: heap is empty")
    }
    new Locator(heapElementsLocators(IndexedHeap.rootIndex))
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
    val first = keyFor(IndexedHeap.rootIndex)
    hashTable.heapIndexes(heapElementsLocators(IndexedHeap.rootIndex)) = HashTableHeapIndexes.reservedMark

    sz -= 1
    if (sz > 0) {
      heapElementsLocators(IndexedHeap.rootIndex) = heapElementsLocators(sz)
      hashTable.heapIndexes(heapElementsLocators(sz)) = IndexedHeap.rootIndex
      heapifyDownFrom(IndexedHeap.rootIndex)
    }
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
    containsIndex(hashTableIndexFor(locator))
}

