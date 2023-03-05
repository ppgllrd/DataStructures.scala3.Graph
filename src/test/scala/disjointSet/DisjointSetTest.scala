package disjointSet

import data.structures.mutable.disjointSet.*
import data.structures.mutable.disjointSet.indexedSet.ArrayIndexedSet

import scala.util.Random

@main def DisjointSetTest(): Unit = {
  val n = 10000000
  val elements = ArrayIndexedSet(Array.range(0, n))


  def run(seed: Int, djs: DisjointSet[Int]): Long = {
    val rnd = Random(seed)
    val steps = 5000000
    val t0 = System.currentTimeMillis()
    for (i <- 0 until steps) {
      val x = rnd.nextInt(n)
      val y = rnd.nextInt(n)
      djs.union(x, y)
      val x1 = rnd.nextInt(n)
      val y1 = rnd.nextInt(n)
      djs.areConnected(x1, y1)
    }
    val t1 = System.currentTimeMillis()
    t1 - t0
  }


  var s1 = 0L
  var s2 = 0L
  val runs = 10
  for (seed <- 0 until runs) {
    val djs1 = WeightedPathCompressedDisjointSet.fromIndexedSet(elements)
    val djs2 = InterleavedDisjointSet.fromIndexedSet(elements)

    s1 += run(seed, djs1)
    s2 += run(seed, djs2)
  }

  val t1 = s1.toDouble / (runs * 1000)
  val t2 = s2.toDouble / (runs * 1000)


  println(t1)
  println(t2)
  println(t1 / t2)


  val djs1 = WeightedPathCompressedDisjointSet.fromIndexedSet(elements)
  val djs2 = InterleavedDisjointSet.fromIndexedSet(elements)
  run(0, djs1)
  run(0, djs2)

  val rnd = Random()
  val nTests = 10000
  for (i <- 0 until nTests) {
    val x = rnd.nextInt(n)
    val y = rnd.nextInt(n)
    assert(djs1.areConnected(x, y) == djs2.areConnected(x, y))
  }
}


@main def ErdosRenyi(): Unit = {
  def runtimeTest[DSI <: DisjointSet[Int]](makeDisjointSet: Int => DSI): Double = {
    def experiment(seed: Int, size: Int): Int = {
      val rnd = new scala.util.Random(seed)

      var edges = 0
      val disjointSet = makeDisjointSet(size)
      while (disjointSet.numberOfComponents > 1) {
        val n = rnd.nextInt(size)
        val m = rnd.nextInt(size)
        disjointSet.union(n, m)
        edges += 1
      }
      edges
    }

    val t0 = System.currentTimeMillis()

    val nExperiments = 5000
    for (size <- List.iterate(100, 8)(_ * 2)) {
      var s = 0
      for (seed <- 0 until nExperiments)
        s += experiment(seed, size)
      val avg = s.toDouble / nExperiments
      println(s"$size:\t$avg")
    }

    val t1 = System.currentTimeMillis()
    val runningTime = (t1 - t0) / 1000.0
    println(s"Running time: $runningTime secs.")
    runningTime
  }

  val tInter = runtimeTest[InterleavedDisjointIntSet](InterleavedDisjointIntSet(_))
  val tWPC = runtimeTest[WeightedPathCompressedDisjointIntSet](WeightedPathCompressedDisjointIntSet(_))
  val tRPC = runtimeTest[RankedPathCompressedDisjointIntSet](RankedPathCompressedDisjointIntSet(_))

  println(f"Interleaved is ${100 * (1 - tInter / tWPC)}%.2f%% faster than weighted")
  println(f"Interleaved is ${100 * (1 - tInter / tRPC)}%.2f%% faster than ranked")
  println(f"Ranked is ${100 * (1 - tRPC / tWPC)}%.2f%% faster than weighted")
}