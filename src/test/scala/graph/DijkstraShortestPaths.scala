package graph

import data.structures.mutable.graph.MapWeightedGraph
import data.structures.mutable.graph.shortestPaths.Dijkstra

import scala.io.Source

@main def DijkstraShortestPaths1(): Unit = {
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
  wg.addEdge('d', 'e', 5)

  // wg.deleteEdge(WeightedEdge('d','e', 5))

  println(wg.edges)

  val source = 'a'
  val dijkstra = Dijkstra(wg, source)
  for (vertex <- wg.vertices)
    println(s"Cost of shortest path from $source to $vertex is ${dijkstra.lowestCostTo(vertex)}")

  println()
  for (vertex <- wg.vertices)
    println(s"Shortest path from $source to $vertex is ${dijkstra.shortestPathTo(vertex)}")
}

@main def DijkstraShortestPaths2(): Unit = {
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
  val source = 'a'
  val dijkstra = Dijkstra(wg, source)
  val t1 = System.currentTimeMillis()
  for (vertex <- wg.vertices)
    println(s"Cost of shortest path from $source to $vertex is ${dijkstra.lowestCostTo(vertex)}")

  println()
  for (vertex <- wg.vertices)
    println(s"Shortest path from $source to $vertex is ${dijkstra.shortestPathTo(vertex)}")

  println()
  val seconds = (t1 - t0) / 1000.0
  println(s"Graph with $order vertices and $size edges solved in $seconds seconds")
}

@main def DijkstraShortestPaths3(): Unit = {
  val p1 =
    """5 7
      |0 1 7
      |0 2 1
      |0 3 2
      |1 2 3
      |1 3 5
      |1 4 1
      |3 4 7""".stripMargin

  val s1 = "0 4 1 2 5"

  val p2 =
    """4 5
      |0 1 5
      |0 2 8
      |1 2 9
      |1 3 2
      |2 3 6""".stripMargin

  val s2 = "0 5 8 7"

  val p3 =
    """9 14
      |0 1 4
      |0 7 8
      |1 2 8
      |1 7 11
      |2 3 7
      |2 5 4
      |2 8 2
      |3 4 9
      |3 5 14
      |4 5 10
      |5 6 2
      |6 7 1
      |6 8 6
      |7  8 7""".stripMargin

  val s3 = "0 4 12 19 21 11 9 8 14"

  val p4 =
    """5 7
      |0 1 7
      |0 2 3
      |1 2 1
      |1 3 2
      |1 4 6
      |2 3 4
      |3 4 4 """.stripMargin

  val s4 = "0 4 3 6 10"

  def solve(problem: String, solution: String): Unit = {
    Source.fromString(problem).getLines.flatMap(_.split("\\W+")).toList.map(_.toInt) match
      case order :: size :: numbers =>
        val wg = MapWeightedGraph[Int, Int]()
        for (vertex <- 0 until order)
          wg.addVertex(vertex)
        for (List(v1, v2, w) <- numbers.grouped(3)) {
          wg.addEdge(v1, v2, w)
        }
        val source = 0
        val dijkstra = Dijkstra(wg, source)
        val costs = for (vertex <- wg.vertices.toList.sorted) yield dijkstra.lowestCostTo(vertex)
        println(costs)
        assert(costs.mkString(" ") == solution)
      case _ => sys.error("wrong problem format")
  }

  solve(p1, s1)
  solve(p2, s2)
  solve(p3, s3)
  solve(p4, s4)
}

@main def DijkstraShortestPaths4(): Unit = {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)

  def solve(path: String): Unit = {
    val wg = ReadWeightedGraph(path)
    val t0 = System.currentTimeMillis()
    val source = 0
    val dijkstra = Dijkstra(wg, source)
    val t1 = System.currentTimeMillis()
    val seconds = (t1 - t0) / 1000.0
    println(path)
    println(s"Graph with ${wg.order} vertices and ${wg.size} edges solved in $seconds seconds")

    val costs = for (vertex <- wg.vertices.toList.sorted) yield dijkstra.lowestCostTo(vertex)
    println(costs)

    for (vertex <- wg.vertices.toList.sorted)
      println(s"$vertex ${dijkstra.shortestPathTo(vertex)}")
  }

  solve("data/tinyEWD.txt")
  println()
}