package collection.mutable

import scala.reflect.ClassTag

object MinHeap {
  private inline val freeMark = -1
  private inline val maximumLoadFactor = 0.5
  private inline val rootIndex = 0
}

class MinHeap[T](initialCapacity: Int)(using priority: Ordering[T], classTag: ClassTag[T]) {
  private class HashTable {
    private var keys = new Array[T](initialCapacity * 2)

    var heapIndexes: Array[Int] = Array.fill[Int](initialCapacity * 2)(MinHeap.freeMark)

    private inline def isFree(inline index: Int): Boolean =
      heapIndexes(index) == MinHeap.freeMark

    private inline def isOccupied(inline index: Int): Boolean =
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

    def heapIndexOf(key: T): Int =
      heapIndexes(indexOf(key))

    private def loadFactor: Double = sz.toDouble / keys.length

    def insert(key: T, heapIndex: Int): (Int, Boolean) = {
      if (loadFactor > MinHeap.maximumLoadFactor) {
        // perform rehashing
        val oldKeys = keys
        val oldHeapIndexes = heapIndexes
        keys = new Array[T](keys.length * 2)
        heapIndexes = Array.fill[Int](heapIndexes.length * 2)(MinHeap.freeMark)
        for (i <- oldKeys.indices) {
          if (isOccupied(i)) {
            val index = indexOf(oldKeys(i))
            keys(index) = oldKeys(i)
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

  private val hashTable = new HashTable
  private var heapElements = new Array[T](initialCapacity)
  private var hashTableIndexes = new Array[Int](initialCapacity)
  private var sz = 0

  def isEmpty: Boolean = sz <= 0

  def nonEmpty: Boolean = sz > 0

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
      // retrieve and update  already inserted element and decrease its priority
      val heapIndex = hashTable.heapIndexes(elementHashTableIndex)
      require(heapElements(heapIndex) == element)
      require(priority.compare(element, heapElements(heapIndex)) <= 0)

      heapElements(heapIndex) = element
      heapifyUpFrom(heapIndex)
    }
  }

  private def heapifyUpFrom(index: Int): Unit = {
    var elementIndex = index
    val elementHashTableIndex = hashTableIndexes(elementIndex)

    var sorted = false
    while (!sorted && elementIndex > MinHeap.rootIndex) {
      val parentIndex = (elementIndex - 1) / 2
      if (priority.compare(heapElements(elementIndex), heapElements(parentIndex)) >= 0) {
        sorted = true
      } else {
        val parentHashTableIndex = hashTable.indexOf(heapElements(parentIndex))

        val tmpElement = heapElements(elementIndex)
        heapElements(elementIndex) = heapElements(parentIndex)
        heapElements(parentIndex) = tmpElement

        val tmpHashTableIndex = hashTableIndexes(elementIndex)
        hashTableIndexes(elementIndex) = hashTableIndexes(parentIndex)
        hashTableIndexes(parentIndex) = tmpHashTableIndex

        val tmpHeapIndex = hashTable.heapIndexes(parentHashTableIndex)
        hashTable.heapIndexes(parentHashTableIndex) = hashTable.heapIndexes(elementHashTableIndex)
        hashTable.heapIndexes(elementHashTableIndex) = tmpHeapIndex

        elementIndex = parentIndex
      }
    }
  }

  private def heapifyDownFrom(index: Int): Unit = {
    var elementIndex = index
    val elementHashTableIndex = hashTableIndexes(elementIndex)

    var sorted = false
    var minimumChildIndex = 0
    while ( {
      minimumChildIndex = elementIndex * 2 + 1
      !sorted && minimumChildIndex < sz
    }) {
      val rightChildIndex = minimumChildIndex + 1
      if (rightChildIndex < sz && priority.compare(heapElements(rightChildIndex), heapElements(minimumChildIndex)) < 0) {
        minimumChildIndex = rightChildIndex
      }

      if (priority.compare(heapElements(minimumChildIndex), heapElements(elementIndex)) >= 0) {
        sorted = true
      } else {
        val minimumChildHashTableIndex = hashTable.indexOf(heapElements(minimumChildIndex))

        val tmpElement = heapElements(elementIndex)
        heapElements(elementIndex) = heapElements(minimumChildIndex)
        heapElements(minimumChildIndex) = tmpElement

        val tmpHashTableIndex = hashTableIndexes(elementIndex)
        hashTableIndexes(elementIndex) = hashTableIndexes(minimumChildIndex)
        hashTableIndexes(minimumChildIndex) = tmpHashTableIndex

        val tmpHeapIndex = hashTable.heapIndexes(minimumChildHashTableIndex)
        hashTable.heapIndexes(minimumChildHashTableIndex) = hashTable.heapIndexes(elementHashTableIndex)
        hashTable.heapIndexes(elementHashTableIndex) = tmpHeapIndex

        elementIndex = minimumChildIndex
      }
    }
  }

  def first: T = {
    if (sz < 1) {
      throw new NoSuchElementException("first: heap is empty")
    }
    heapElements(MinHeap.rootIndex)
  }

  def deleteFirst(): T = {
    if (sz < 1) {
      throw new NoSuchElementException("deleteFirst: heap is empty")
    }
    val first = heapElements(MinHeap.rootIndex)
    sz -= 1
    heapElements(0) = heapElements(sz)
    hashTableIndexes(0) = hashTableIndexes(sz)
    heapifyDownFrom(0)

    heapElements(sz) = null.asInstanceOf[T] // let GC reclaim memory

    first
  }
}

