package graph

import data.structures.mutable.heap.MinHeapMap

object MinHeap extends App {
  case class Element(val key: Int, val priority: Int) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[Element]

    override def equals(other: Any): Boolean = other match {
      case that: Element =>
        (that canEqual this) && key == that.key
      case _ => false
    }

    override def hashCode(): Int =
      key.hashCode()
  }

  val priority = Ordering.by[Element, Int](_.priority)

  val heap = new MinHeapMap[Element, Unit](100)(using priority)

  val rnd = scala.util.Random(0)
  val xs = Array.fill(100) {
    val x = rnd.nextInt(1000)
    Element(x, x)
  }.toSet

  println(xs.size)
  for (x <- xs)
    heap.insert(x)

  heap.insert(Element(957, 940)) // improve priority
  heap.insert(Element(960, 902)) // improve priority
  heap.insert(Element(947, 950)) // worsen priority

  var prev = -1
  while (heap.nonEmpty) {
    val x = heap.extractFirst()
    println(x.toString + " " + (x.priority >= prev))
    prev = x.priority
  }
}