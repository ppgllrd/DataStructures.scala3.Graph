package collection.mutable

import scala.reflect.ClassTag

class Locator private[mutable](val index: Int) extends AnyVal

object MinUpdatableHeap {
  private inline val freeMark = -1
  private inline val extractedMark = -2
  private inline val maximumLoadFactor = 0.5
  private inline val rootIndex = 0
  private inline val defaultCapacity = 128

  def apply[T](initialCapacity: Int)(using priority: Ordering[T])(using classTag: ClassTag[T]): MinUpdatableHeap[T] =
    new MinUpdatableHeap(initialCapacity)(using priority)(using classTag)

  def apply[T](using priority: Ordering[T])(using classTag: ClassTag[T]): MinUpdatableHeap[T] =
    new MinUpdatableHeap(defaultCapacity)(using priority)(using classTag)
}

class MinUpdatableHeap[T](initialCapacity: Int)(using priority: Ordering[T])(using classTag: ClassTag[T]) {
  private class HashTable {
    var keys = new Array[T](initialCapacity * 2)

    var heapIndexes: Array[Int] = Array.fill[Int](initialCapacity * 2)(MinUpdatableHeap.freeMark)

    inline def isFree(inline index: Int): Boolean =
      heapIndexes(index) == MinUpdatableHeap.freeMark

    inline def isExtracted(inline index: Int): Boolean =
      heapIndexes(index) == MinUpdatableHeap.extractedMark

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
      if (loadFactor > MinUpdatableHeap.maximumLoadFactor) {
        // perform rehashing
        val oldKeys = keys
        val oldHeapIndexes = heapIndexes
        keys = new Array[T](keys.length * 2)
        heapIndexes = Array.fill[Int](heapIndexes.length * 2)(MinUpdatableHeap.freeMark)
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
      // retrieve and update already inserted element and decrease its priority
      val heapIndex = hashTable.heapIndexes(elementHashTableIndex)
      require(heapElements(heapIndex) == element, s"${heapElements(heapIndex)} $element")
      require(priority.compare(element, heapElements(heapIndex)) <= 0)

      heapElements(heapIndex) = element
      heapifyUpFrom(heapIndex)
    }
    // println(s"insert $element")
    check()
  }

  private inline def parentIndexOf(inline index: Int): Int =
    (index - 1) / 2

  private inline def leftChildIndexOf(inline index: Int): Int =
    index * 2 + 1

  private inline def isValidIndex(inline index: Int): Boolean =
    index < sz

  private inline def hasParentIndex(inline index: Int): Boolean =
    index > MinUpdatableHeap.rootIndex

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
        /*
                val parentHashTableIndex = hashTableIndexes(parentIndex)

                val tmpElement = heapElements(elementIndex)
                heapElements(elementIndex) = heapElements(parentIndex)
                heapElements(parentIndex) = tmpElement

                val tmpHashTableIndex = hashTableIndexes(elementIndex)
                hashTableIndexes(elementIndex) = parentHashTableIndex
                hashTableIndexes(parentIndex) = tmpHashTableIndex

                val tmpHeapIndex = hashTable.heapIndexes(parentHashTableIndex)
                hashTable.heapIndexes(parentHashTableIndex) = elementIndex
                hashTable.heapIndexes(elementHashTableIndex) = tmpHeapIndex

                elementIndex = parentIndex
        */
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
        /*
                val minimumChildHashTableIndex = hashTableIndexes(minimumChildIndex)

                val tmpElement = heapElements(elementIndex)
                heapElements(elementIndex) = heapElements(minimumChildIndex)
                heapElements(minimumChildIndex) = tmpElement

                val tmpHashTableIndex = hashTableIndexes(elementIndex)
                hashTableIndexes(elementIndex) = minimumChildHashTableIndex
                hashTableIndexes(minimumChildIndex) = tmpHashTableIndex

                val tmpHeapIndex = hashTable.heapIndexes(minimumChildHashTableIndex)
                hashTable.heapIndexes(minimumChildHashTableIndex) = elementIndex
                hashTable.heapIndexes(elementHashTableIndex) = tmpHeapIndex

                elementIndex = minimumChildIndex
        */
      }
    }
    if (moved) {
      // place initial element in its right position
      heapElements(elementIndex) = element
      hashTableIndexes(elementIndex) = elementHashTableIndex

      hashTable.heapIndexes(elementHashTableIndex) = elementIndex
    }
  }

  def first: T = {
    if (sz < 1) {
      throw new NoSuchElementException("first: heap is empty")
    }
    heapElements(MinUpdatableHeap.rootIndex)
  }

  def deleteFirst(): T = {
    if (sz < 1) {
      throw new NoSuchElementException("deleteFirst: heap is empty")
    }
    // println("deleteFirst")
    check()
    val first = heapElements(MinUpdatableHeap.rootIndex)
    hashTable.heapIndexes(hashTableIndexes(MinUpdatableHeap.rootIndex)) = MinUpdatableHeap.extractedMark

    sz -= 1
    heapElements(0) = heapElements(sz)
    hashTableIndexes(0) = hashTableIndexes(sz)
    hashTable.heapIndexes(hashTableIndexes(sz)) = 0

    heapifyDownFrom(0)

    heapElements(sz) = null.asInstanceOf[T] // let GC reclaim memory
    // println("deleteFirst")
    check()

    first
  }

  def locatorFor(element: T): Locator = {
    val hashTableIndex = hashTable.indexOf(element)
    new Locator(hashTableIndex)
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

  def search(element: T): Option[T] = {
    val hashTableIndex = hashTable.indexOf(element)

    if (hashTable.isExtracted(hashTableIndex)) {
      None
    } else if (hashTable.isOccupied(hashTableIndex)) {
      Some(heapElements(hashTable.heapIndexes(hashTableIndex)))
    } else {
      None
    }
  }
}

