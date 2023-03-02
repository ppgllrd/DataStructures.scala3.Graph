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
 * This hash table stores elements in heap as keys and their index within the heap (for fast lookup of such elements).
 *
 * @param initialCapacity initial capacity of hash table. Will be expanded as needed.
 * @param classTagT       a class tag for type of keys stored in hash table.
 * @tparam T Type of elements stored in heap.
 * @author Pepe Gallardo
 */
private[heap] class HashTable[T](initialCapacity: Int)(using
  classTagT: ClassTag[T]) {
  // elements in heap which are also keys in dictionary
  var keys = new Array[T](initialCapacity)
  // indexes for locating each key in heap
  var heapIndexes: Array[Int] = Array.fill[Int](initialCapacity)(HashTable.freeMark)

  // index of key for each locator
  val locators: ArrayBuffer[Int] = ArrayBuffer[Int]()

  // index of locator for each key
  var srotacol: Array[Int] = Array.fill[Int](initialCapacity)(HashTable.noLocator)

  // initially all cells are free
  inline def isFree(inline array: Array[Int], inline index: Int): Boolean =
    array(index) == HashTable.freeMark

  inline def isFree(inline index: Int): Boolean =
    isFree(heapIndexes, index)

  // a cell is reserved if it stores an element that was extracted from heap or if it is reserved for inserting an
  // element that has already an associated locator
  inline def isReserved(inline index: Int): Boolean =
    heapIndexes(index) == HashTable.reservedMark

  // a cell is occupied if it corresponds to an element in heap (heapIndexes > 0) or if it is reserved
  inline def isOccupied(inline index: Int): Boolean =
    !isFree(index)

  inline def isInHeap(inline array: Array[Int], inline index: Int): Boolean =
    array(index) >= 0

  inline def isInHeap(inline index: Int): Boolean =
    isInHeap(heapIndexes, index)

  // key with provided index has not yet a locator
  inline def noLocator(inline index: Int): Boolean =
    srotacol(index) == HashTable.noLocator

  // number of cells which are occupied in hash table
  private var sz = 0

  // computes hash of an element
  private def hash(key: T): Int = (key.hashCode() & 0x7fffffff) % keys.length

  // searches for index of an element in the hash table
  def searchIndexOf(key: T): Int = {
    var index = hash(key)
    while (isOccupied(index) && keys(index) != key) {
      index = (index + 1) % keys.length
    }
    index
  }

  // computes current load factor of hash table
  protected def loadFactor: Double = sz.toDouble / keys.length

  // performs rehashing if current load factor exceeds maximum allowed one
  private[heap] def rehashing(): Boolean = {
    var rehashed = false
    if (loadFactor > HashTable.maximumLoadFactor) {
      val oldKeys = keys
      val oldHeapIndexes = heapIndexes
      val oldSrotacol = srotacol
      keys = new Array[T](keys.length * 2)
      heapIndexes = Array.fill[Int](heapIndexes.length * 2)(HashTable.freeMark)
      srotacol = Array.fill[Int](srotacol.length * 2)(HashTable.noLocator)
      for (i <- oldKeys.indices) {
        if (oldHeapIndexes(i) != HashTable.freeMark) {
          if(oldKeys(i) == null)
            println(s"${oldHeapIndexes(i)}")
          val index = searchIndexOf(oldKeys(i))
          keys(index) = oldKeys(i)
          heapIndexes(index) = oldHeapIndexes(i)
          srotacol(index) = oldSrotacol(i)
        }
      }
      rehashed = true
    }
    rehashed
  }

  /**
   * Inserts an element and its associated index in heap in hash table. Operation is O(log n).
   *
   * @param index     index in hash table where element should be stored (according to its hash code).
   * @param key       element to insert in hash table.
   * @param heapIndex index in heap where this element is stored.
   * @return `true` if element was inserted or `false` if element was already in hash table.
   */
  def insert(index: Int, key: T, heapIndex: Int): (Boolean, Int) = {
    var inserted = false
    var locatorIndex = srotacol(index)
    if(isFree(index))
      sz += 1
    if (isFree(index) || isReserved(index)) {
      keys(index) = key
      heapIndexes(index) = heapIndex
      if (locatorIndex == HashTableHeapIndexes.noLocator) {
        // allocate a new Locator
        locatorIndex = locators.length
        locators.addOne(index)
      }
      inserted = true
    }
    (inserted, locatorIndex)
  }

  def insert(key: T, heapIndex: Int): (Boolean, Int, Int) = {
    rehashing()

    val index = searchIndexOf(key)
    val (inserted, locatorIndex) = insert(index, key, heapIndex)
    (inserted, locatorIndex, index)
  }

  def findOrReserve(element: T): Int = {
    rehashing()

    val index = searchIndexOf(element)
    if (isFree(index)) {
      keys(index) = element
      heapIndexes(index) = HashTable.reservedMark
      sz += 1
    }
    index
  }

  def locatorFor(hashTableIndex: Int): Locator = {
    val locatorIndex = srotacol(hashTableIndex)
    if (locatorIndex == HashTableHeapIndexes.noLocator) {
      // allocate a new Locator
      val locatorIndex = locators.length
      locators.addOne(hashTableIndex)
      new Locator(locatorIndex)
    } else {
      // use already existing locator
      new Locator(locatorIndex)
    }
  }

  /**
   * Returns a locator for provided heap element. A locator can later be used for fast access to the element both in
   * the heap and in the map. Operation is O(1) effective.
   *
   * @param element a heap element
   * @return a locator for provided heap element.
   */
  def locatorFor(element: T): Locator = {
    val hashTableIndex = findOrReserve(element)
    locatorFor(hashTableIndex)
  }

  def keyForLocator(locatorIndex: Int): T =
    keys(locators(locatorIndex))
}
