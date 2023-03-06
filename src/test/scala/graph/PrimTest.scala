package graph

import data.structures.mutable.graph.MapWeightedGraph
import data.structures.mutable.graph.connectivity.Connectivity
import data.structures.mutable.graph.cycle.CycleDetector
import data.structures.mutable.graph.minimumSpanningTree.Prim

import scala.io.Source

@main def PrimTest1(): Unit = {
  val wg = new MapWeightedGraph[Char, Int]()
  wg.addVertex('a')
  wg.addVertex('b')
  wg.addVertex('c')
  wg.addVertex('d')
  wg.addVertex('e')

  wg.addEdge('a', 'b', 3)
  wg.addEdge('a', 'd', 7)
  wg.addEdge('b', 'c', 4)
  wg.addEdge('b', 'd', 2)
  wg.addEdge('c', 'd', 5)
  wg.addEdge('c', 'e', 6)
  wg.addEdge('d', 'e', 8)

  val prim = Prim(wg)
  println(prim.minimumSpanningTree.edges)
  println(prim.minimumSpanningTree.edges.toList.map(_.weight).sum)
}

@main def PrimTest2(): Unit = {
  val order = 100000
  val size = 5000000

  val rnd = scala.util.Random()

  val wg = new MapWeightedGraph[Int, Double]()
  for (vertex <- 0 until order) {
    wg.addVertex(vertex)
  }

  for (_ <- 0 until size) {
    val v1 = rnd.nextInt(wg.order)
    val v2 = rnd.nextInt(wg.order)
    val w = 1000 * rnd.nextDouble()
    wg.addEdge(v1, v2, w)
  }

  val t0 = System.currentTimeMillis()
  val prim = Prim(wg)
  val t1 = System.currentTimeMillis()
  val seconds = (t1 - t0) / 1000.0
  println(s"Graph with $order vertices and $size edges solved in $seconds seconds")

  val mst = prim.minimumSpanningTree

  // two alternative tests to check that a graph is a tree
  val isTree1 =
    mst.size == mst.order - 1 && CycleDetector(mst).isAcyclic

  val isTree2 =
    mst.size == mst.order - 1 && Connectivity(mst).isConnected

  // two alternative tests to check that a mst is a spanning tree for wg
  val isMst1 = isTree1 && mst.order == wg.order
  val isMst2 = isTree2 && mst.order == wg.order

  assert(isMst1)
  assert(isMst2)
}

@main def PrimTest3(): Unit = {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)

  def solve(path: String): Unit = {
    val wg = ReadWeightedGraph(path)
    val t0 = System.currentTimeMillis()
    val prim = Prim(wg)
    val t1 = System.currentTimeMillis()
    val seconds = (t1 - t0) / 1000.0
    println(path)
    println(s"Graph with ${wg.order} vertices and ${wg.size} edges solved in $seconds seconds")
    val mst = prim.minimumSpanningTree
    val weight = mst.edges.foldLeft(0.0)((ac, we) => ac + we.weight)

    println(s"MST weight is $weight")
    println(s"MST has ${mst.order} vertices and ${mst.size} edges")
  }

  solve("data/tinyEWG.txt")
  println()
  solve("data/mediumEWG.txt")
  println()
  solve("data/largeEWG.txt")
}