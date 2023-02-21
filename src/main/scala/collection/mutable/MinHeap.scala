package collection.mutable

import scala.reflect.ClassTag

class MinHeap[A](initialCapacity: Int)(using priority: Ordering[A], classTag: ClassTag[A]) {
  private class HashTable {
    private var keys = new Array[A](initialCapacity * 2)
    private val free = -1
    var heapIndexes: Array[Int] = Array.fill[Int](initialCapacity * 2)(free)
    private var sz = 0

    private inline def isFree(inline index: Int): Boolean =
      heapIndexes(index) == free

    private def hash(key: A): Int = (key.hashCode() & 0x7fffffff) % keys.length

    def indexOf(key: A): Int = {
      var index = hash(key)
      while (!isFree(index) && keys(index) != key) {
        index = (index + 1) % keys.length
      }
      index
    }

    def searchHeapIndex(key: A): Int =
      heapIndexes(indexOf(key))

    private def loadFactor: Double = sz.toDouble / keys.length

    def insert(key: A, heapIndex: Int): (Int, Boolean) = {
      val maximumLoadFactor = 0.5
      if (loadFactor > maximumLoadFactor) {
        val oldKeys = keys
        val oldHeapIndexes = heapIndexes
        keys = new Array[A](keys.length * 2)
        heapIndexes = Array.fill[Int](heapIndexes.length * 2)(free)
        for (i <- oldKeys.indices) {
          if (!isFree(i)) {
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
  private var elements = new Array[A](initialCapacity)
  private var hashTableIndexes = new Array[Int](initialCapacity)
  private var sz = 0

  def isEmpty: Boolean = sz <= 0

  def nonEmpty: Boolean = sz > 0

  def insertOrDecrease(element: A): Unit = {
    if (sz >= elements.length) {
      elements = Array.copyOf(elements, elements.length * 2)
      hashTableIndexes = Array.copyOf(hashTableIndexes, hashTableIndexes.length * 2)
    }
    val (hashTableElementIdx, inserted) = hashTable.insert(element, sz)

    if (inserted) {
      elements(sz) = element
      hashTableIndexes(sz) = hashTableElementIdx
      heapifyUp(sz)
      sz += 1
    } else {
      val heapIndex = hashTable.heapIndexes(hashTableElementIdx)
      require(elements(heapIndex) == element)
      require(priority.compare(element, elements(heapIndex)) <= 0)

      elements(heapIndex) = element
      heapifyUp(heapIndex)
    }
  }

  private def heapifyUp(index: Int): Unit = {
    var indexElement = index
    val hashTableElementIdx = hashTableIndexes(indexElement)

    var sorted = false
    while (!sorted && indexElement > 0) {
      val indexParent = (indexElement - 1) / 2
      if (priority.compare(elements(indexElement), elements(indexParent)) >= 0) {
        sorted = true
      } else {
        val hashTableParentIdx = hashTable.indexOf(elements(indexParent))

        val tmpElement = elements(indexElement)
        elements(indexElement) = elements(indexParent)
        elements(indexParent) = tmpElement

        val tmpHashTableIdx = hashTableIndexes(indexElement)
        hashTableIndexes(indexElement) = hashTableIndexes(indexParent)
        hashTableIndexes(indexParent) = tmpHashTableIdx

        val tmpHeapIdx = hashTable.heapIndexes(hashTableParentIdx)
        hashTable.heapIndexes(hashTableParentIdx) = hashTable.heapIndexes(hashTableElementIdx)
        hashTable.heapIndexes(hashTableElementIdx) = tmpHeapIdx

        indexElement = indexParent
      }
    }
  }

  private def heapifyDown(index: Int): Unit = {
    var indexElement = index
    val hashTableElementIdx = hashTableIndexes(indexElement)

    var sorted = false
    var indexMinimumChild = 0
    while ( {
      indexMinimumChild = indexElement * 2 + 1; !sorted && indexMinimumChild < sz
    }) {
      val indexRightChild = indexMinimumChild + 1
      if (indexRightChild < sz && priority.compare(elements(indexRightChild), elements(indexMinimumChild)) < 0) {
        indexMinimumChild = indexRightChild
      }

      if (priority.compare(elements(indexMinimumChild), elements(indexElement)) >= 0) {
        sorted = true
      } else {
        val hashTableMinimumChildIdx = hashTable.indexOf(elements(indexMinimumChild))

        val tmpElement = elements(indexElement)
        elements(indexElement) = elements(indexMinimumChild)
        elements(indexMinimumChild) = tmpElement

        val tmpHashTableIdx = hashTableIndexes(indexElement)
        hashTableIndexes(indexElement) = hashTableIndexes(indexMinimumChild)
        hashTableIndexes(indexMinimumChild) = tmpHashTableIdx

        val tmpHeapIdx = hashTable.heapIndexes(hashTableMinimumChildIdx)
        hashTable.heapIndexes(hashTableMinimumChildIdx) = hashTable.heapIndexes(hashTableElementIdx)
        hashTable.heapIndexes(hashTableElementIdx) = tmpHeapIdx

        indexElement = indexMinimumChild
      }
    }
  }

  def first: A = {
    if (sz < 1) {
      throw new NoSuchElementException("first: heap is empty")
    }
    elements(0)
  }

  def deleteFirst(): A = {
    if (sz < 1) {
      throw new NoSuchElementException("deleteFirst: heap is empty")
    }
    val first = elements(0)
    sz -= 1
    elements(0) = elements(sz)
    hashTableIndexes(0) = hashTableIndexes(sz)
    heapifyDown(0)

    elements(sz) = null.asInstanceOf[A]

    first
  }
}


object TestHeap extends App {
  val heap = new MinHeap[Int](100)

  val rnd = scala.util.Random(0)
  val xs = Array.fill[Int](100)(rnd.nextInt(1000)).toSet
  println(xs.size)
  for (x <- xs)
    heap.insertOrDecrease(x)

  var prev = -1
  while (heap.nonEmpty) {
    val x = heap.deleteFirst()
    println(x + " " + (x >= prev))
    prev = x
  }
}