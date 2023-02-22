package collection.mutable

import scala.reflect.ClassTag

class Locator private[mutable](val index: Int) extends AnyVal

object MinHeapMap {
  private inline val freeMark = -1
  private inline val extractedMark = -2
  private inline val maximumLoadFactor = 0.5
  private inline val rootIndex = 0
  private inline val defaultCapacity = 128

  def apply[T, V](initialCapacity: Int)(using priority: Ordering[T])(using classTagT: ClassTag[T])(using
    classTagV: ClassTag[V]
  )
  : MinHeapMap[T, V] =
    new MinHeapMap(initialCapacity)(using priority)(using classTagT)(using classTagV)

  def apply[T, V](using priority: Ordering[T])(using classTagT: ClassTag[T])(using classTagV: ClassTag[V])
  : MinHeapMap[T, V] =
    new MinHeapMap(defaultCapacity)(using priority)(using classTagT)(using classTagV)
}

class MinHeapMap[T, V](initialCapacity: Int)(using priority: Ordering[T])
  (using classTagT: ClassTag[T])(using classTagV: ClassTag[V]) {
  private class HashTable {
    var keys = new Array[T](initialCapacity * 2)
    var values = new Array[V](initialCapacity * 2)

    var heapIndexes: Array[Int] = Array.fill[Int](initialCapacity * 2)(MinHeapMap.freeMark)

    inline def isFree(inline index: Int): Boolean =
      heapIndexes(index) == MinHeapMap.freeMark

    inline def isExtracted(inline index: Int): Boolean =
      heapIndexes(index) == MinHeapMap.extractedMark

    inline def isOccupied(inline index: Int): Boolean =
      !isFree(index)

    private var sz = 0

    private def hash(key: T): Int = (key.hashCode() & 0x7fffffff) % keys.length

    def indexOf(key: T): Int = {
      var index = hash(key)
      while (isOccupied(index) && keys(index) != key) {
        index = (index + 1) % keys.length
      }
      index
    }

    private def loadFactor: Double = sz.toDouble / keys.length

    def insert(key: T, heapIndex: Int): (Int, Boolean) = {
      if (loadFactor > MinHeapMap.maximumLoadFactor) {
        // perform rehashing
        val oldKeys = keys
        val oldValues = values
        val oldHeapIndexes = heapIndexes
        keys = new Array[T](keys.length * 2)
        values = new Array[V](keys.length * 2)
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

      val index = indexOf(key)
      var inserted = false
      if (isFree(index)) {
        keys(index) = key
        heapIndexes(index) = heapIndex
        inserted = true
        sz += 1
      }
      (index, inserted)
    }
  }

  private def check(): Unit = {
    for (i <- 0 until sz) {
      assert(heapElements(i) == hashTable.keys(hashTableIndexes(i)),
        s"${heapElements(i)} ${hashTable.keys(hashTableIndexes(i))}")
    }
  }

  private val hashTable = new HashTable
  private var heapElements = new Array[T](initialCapacity)
  private var hashTableIndexes = new Array[Int](initialCapacity)
  private var sz = 0

  private inline def parentIndexOf(inline index: Int): Int =
    (index - 1) / 2

  private inline def leftChildIndexOf(inline index: Int): Int =
    index * 2 + 1

  private inline def isValidIndex(inline index: Int): Boolean =
    index < sz

  private inline def hasParentIndex(inline index: Int): Boolean =
    index > MinHeapMap.rootIndex

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

  def isEmpty: Boolean = sz <= 0

  def nonEmpty: Boolean = sz > 0

  def locatorFor(element: T): Locator = {
    val hashTableIndex = hashTable.indexOf(element)
    if (hashTable.isFree(hashTableIndex)) {
      hashTable.keys(hashTableIndex) = element
    }
    new Locator(hashTableIndex)
  }

  def insertOrIncreasePriority(element: T): Unit = {
    val heapIndex = sz
    if (heapIndex >= heapElements.length) {
      // ensure capacity
      heapElements = Array.copyOf(heapElements, heapElements.length * 2)
      hashTableIndexes = Array.copyOf(hashTableIndexes, hashTableIndexes.length * 2)
    }
    val (elementHashTableIndex, inserted) = hashTable.insert(element, heapIndex)

    if (inserted) {
      // insert new element in heap
      heapElements(heapIndex) = element
      hashTableIndexes(heapIndex) = elementHashTableIndex
      heapifyUpFrom(heapIndex)
      sz += 1
    } else {
      // retrieve and update already inserted element and decrease its priority
      val heapIndex = hashTable.heapIndexes(elementHashTableIndex)
      require(heapElements(heapIndex) == element, s"${heapElements(heapIndex)} $element")
      require(priority.compare(element, heapElements(heapIndex)) <= 0)

      heapElements(heapIndex) = element
      heapifyUpFrom(heapIndex)
    }
  }

  def first: T = {
    if (sz < 1) {
      throw new NoSuchElementException("first: heap is empty")
    }
    heapElements(MinHeapMap.rootIndex)
  }

  def deleteFirst(): T = {
    if (sz < 1) {
      throw new NoSuchElementException("deleteFirst: heap is empty")
    }
    val first = heapElements(MinHeapMap.rootIndex)
    hashTable.heapIndexes(hashTableIndexes(MinHeapMap.rootIndex)) = MinHeapMap.extractedMark

    sz -= 1
    heapElements(0) = heapElements(sz)
    hashTableIndexes(0) = hashTableIndexes(sz)
    hashTable.heapIndexes(hashTableIndexes(sz)) = 0

    heapifyDownFrom(0)

    heapElements(sz) = null.asInstanceOf[T] // let GC reclaim memory

    first
  }

  def update(element: T, value: V): Locator = {
    val hashTableIndex = hashTable.indexOf(element)
    if (hashTable.isFree(hashTableIndex)) {
      hashTable.keys(hashTableIndex) = element
    }
    hashTable.values(hashTableIndex) = value
    new Locator(hashTableIndex)
  }

  def update(locator: Locator, value: V): Locator = {
    val hashTableIndex = locator.index
    (!hashTable.isFree(hashTableIndex), "update: element for locator is not in map")
    hashTable.values(hashTableIndex) = value
    locator
  }

  def apply(element: T): V = {
    val hashTableIndex = hashTable.indexOf(element)
    assert(!hashTable.isFree(hashTableIndex), "getValue: element for locator is not in map")
    hashTable.values(hashTableIndex)
  }

  def apply(locator: Locator): V = {
    val hashTableIndex = locator.index
    assert(!hashTable.isFree(hashTableIndex), "getValue: element for locator is not in map")
    hashTable.values(hashTableIndex)
  }

  def get(element: T): Option[V] = {
    val hashTableIndex = hashTable.indexOf(element)
    if (!hashTable.isFree(hashTableIndex)) {
      Some(hashTable.values(hashTableIndex))
    } else {
      None
    }
  }

  def get(locator: Locator): Option[V] = {
    val hashTableIndex = locator.index
    if (!hashTable.isFree(hashTableIndex)) {
      Some(hashTable.values(hashTableIndex))
    } else {
      None
    }
  }

  def contains(element: T): Boolean = {
    val hashTableIndex = hashTable.indexOf(element)
    hashTable.isOccupied(hashTableIndex)
  }

  def contained(element: T): Boolean = {
    val hashTableIndex = hashTable.indexOf(element)
    hashTable.isExtracted(hashTableIndex)
  }

  def hasContained(element: T): Boolean = {
    val hashTableIndex = hashTable.indexOf(element)
    hashTable.isExtracted(hashTableIndex) || hashTable.isOccupied(hashTableIndex)
  }
}

