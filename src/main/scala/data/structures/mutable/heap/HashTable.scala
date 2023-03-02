package data.structures.mutable.heap

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object HashTable {
  private[heap] inline val freeMark = -1
  private[heap] inline val reservedMark = -2
  private[heap] inline val noLocator = -3

  private[heap] inline val maximumLoadFactor = 0.5
}

/**
 * This hash table stores elements in heap as keys and their hashTableIndex within the heap (for fast lookup of such elements).
 *
 * @param initialCapacity initial capacity of hash table. Will be expanded as needed.
 * @param classTagT       a class tag for type of keys stored in hash table.
 * @tparam T Type of elements stored in heap.
 * @author Pepe Gallardo
 */
private[heap] class HashTable[T](initialCapacity: Int)(using classTagT: ClassTag[T]) {
  // hashTableIndex in hash table for each locator
  val locatorToHashTableIndexes: ArrayBuffer[Int] = ArrayBuffer[Int]()
  // elements in heap which are also keys in dictionary
  var keys = new Array[T](initialCapacity)
  // indexes for locating each key in heap
  var heapIndexes: Array[Int] = Array.fill[Int](initialCapacity)(HashTable.freeMark)
  // hashTableIndex of locator for hash table hashTableIndex
  var hashTableIndexToLocators: Array[Int] = Array.fill[Int](initialCapacity)(HashTable.noLocator)
  // number of cells which are occupied in hash table
  private var sz = 0

  // initially all cells are free
  inline def isFree(inline array: Array[Int], inline hashTableIndex: Int): Boolean =
    array(hashTableIndex) == HashTable.freeMark

  inline def isInHeap(inline array: Array[Int], inline hashTableIndex: Int): Boolean =
    array(hashTableIndex) >= 0

  inline def isInHeap(inline hashTableIndex: Int): Boolean =
    isInHeap(heapIndexes, hashTableIndex)

  // key with provided hashTableIndex has not yet a locator
  inline def noLocator(inline hashTableIndex: Int): Boolean =
    hashTableIndexToLocators(hashTableIndex) == HashTable.noLocator

  def insert(key: T, heapIndex: Int): (Boolean, Int, Int) = {
    rehashing()

    val hashTableIndex = searchHashTableIndexOf(key)
    val (inserted, locatorIndex) = insert(hashTableIndex, key, heapIndex)
    (inserted, locatorIndex, hashTableIndex)
  }

  // performs rehashing if current load factor exceeds maximum allowed one
  private[heap] def rehashing(): Boolean = {
    var rehashed = false
    if (loadFactor > HashTable.maximumLoadFactor) {
      val oldKeys = keys
      val oldHeapIndexes = heapIndexes
      val oldHashTableIndexToLocators = hashTableIndexToLocators
      keys = new Array[T](keys.length * 2)
      heapIndexes = Array.fill[Int](heapIndexes.length * 2)(HashTable.freeMark)
      hashTableIndexToLocators = Array.fill[Int](hashTableIndexToLocators.length * 2)(HashTable.noLocator)
      for (i <- oldKeys.indices) {
        if (oldHeapIndexes(i) != HashTable.freeMark) {
          if (oldKeys(i) == null)
            println(s"${oldHeapIndexes(i)}")
          val hashTableIndex = searchHashTableIndexOf(oldKeys(i))
          keys(hashTableIndex) = oldKeys(i)
          heapIndexes(hashTableIndex) = oldHeapIndexes(i)
          locatorToHashTableIndexes(oldHashTableIndexToLocators(i)) = hashTableIndex
          hashTableIndexToLocators(hashTableIndex) = oldHashTableIndexToLocators(i)
        }
      }
      rehashed = true
    }
    rehashed
  }

  // searches for hashTableIndex of an element in the hash table
  def searchHashTableIndexOf(key: T): Int = {
    var hashTableIndex = hash(key)
    while (isOccupied(hashTableIndex) && keys(hashTableIndex) != key) {
      hashTableIndex = (hashTableIndex + 1) % keys.length
    }
    hashTableIndex
  }

  // a cell is occupied if it corresponds to an element in heap (heapIndexes > 0) or if it is reserved
  inline def isOccupied(inline hashTableIndex: Int): Boolean =
    !isFree(hashTableIndex)

  // computes hash of an element
  private def hash(key: T): Int = (key.hashCode() & 0x7fffffff) % keys.length

  // computes current load factor of hash table
  protected def loadFactor: Double = sz.toDouble / keys.length

  /**
   * Inserts an element and its associated hashTableIndex in heap in hash table. Operation is O(log n).
   *
   * @param hashTableIndex hashTableIndex in hash table where element should be stored (according to its hash code).
   * @param key            element to insert in hash table.
   * @param heapIndex      hashTableIndex in heap where this element is stored.
   * @return `true` if element was inserted or `false` if element was already in hash table.
   */
  def insert(hashTableIndex: Int, key: T, heapIndex: Int): (Boolean, Int) = {
    var inserted = false

    if (isFree(hashTableIndex))
      sz += 1

    var locatorIndex = hashTableIndexToLocators(hashTableIndex)

    println(locatorIndex)
    println(isFree(hashTableIndex))
    println(isReserved(hashTableIndex))
    if (isFree(hashTableIndex) || isReserved(hashTableIndex)) {
      keys(hashTableIndex) = key
      heapIndexes(hashTableIndex) = heapIndex

      if (locatorIndex == HashTableHeapIndexes.noLocator) {
        // allocate a new Locator
        locatorIndex = locatorToHashTableIndexes.length
        println(locatorIndex)
        locatorToHashTableIndexes.addOne(hashTableIndex)
        hashTableIndexToLocators(hashTableIndex) = locatorIndex
      }
      inserted = true
    } else {
      // update key
      keys(hashTableIndex) = key
    }
    (inserted, locatorIndex)
  }

  inline def isFree(inline hashTableIndex: Int): Boolean =
    isFree(heapIndexes, hashTableIndex)

  // a cell is reserved if it stores an element that was extracted from heap or if it is reserved for inserting an
  // element that has already an associated locator
  inline def isReserved(inline hashTableIndex: Int): Boolean =
    heapIndexes(hashTableIndex) == HashTable.reservedMark

  /**
   * Returns a locator for provided heap element. A locator can later be used for fast access to the element both in
   * the heap and in the map. Operation is O(1) effective.
   *
   * @param element a heap element
   * @return a locator for provided heap element.
   */
  def locatorFor(element: T): Locator = {
    rehashing()

    val hashTableIndex = searchHashTableIndexOf(element)
    if (isFree(hashTableIndex)) {
      keys(hashTableIndex) = element
      heapIndexes(hashTableIndex) = HashTable.reservedMark
      sz += 1
    }
    locatorFor(hashTableIndex)
  }

  private def locatorFor(hashTableIndex: Int): Locator = {
    val locatorIndex = hashTableIndexToLocators(hashTableIndex)
    if (locatorIndex == HashTable.noLocator) {
      // allocate a new Locator
      val locatorIndex = locatorToHashTableIndexes.length
      locatorToHashTableIndexes.addOne(hashTableIndex)
      hashTableIndexToLocators(hashTableIndex) = locatorIndex
      new Locator(locatorIndex)
    } else {
      // use already existing locator
      new Locator(locatorIndex)
    }
  }

  def keyForLocator(locatorIndex: Int): T =
    keys(locatorToHashTableIndexes(locatorIndex))
}
