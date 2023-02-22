package graph

import collection.mutable.MinHeapMap


object MinHeap extends App {
  case class Element(key: Int, priority: Int)

  val priority = Ordering.by[Element, Int](_.priority)

  val heap = new MinHeapMap[Element, Int](100)(using priority)

  val rnd = scala.util.Random(0)
  val xs = Array.fill(100) {
    val x = rnd.nextInt(1000)
    Element(x, x)
  }.toSet

  println(xs.size)
  for (x <- xs)
    heap.insertOrIncreasePriority(x)

  heap.insertOrIncreasePriority(Element(957, 940))
  heap.insertOrIncreasePriority(Element(960, 902))

  var prev = -1
  while (heap.nonEmpty) {
    val x = heap.deleteFirst()
    println(x.toString + " " + (x.priority >= prev))
    prev = x.priority
  }
}