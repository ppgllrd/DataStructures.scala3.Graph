package graph

import data.structures.mutable.graph.{Edge, MapGraph, MapWeightedGraph, WeightedEdge}

object Test extends App {
  val g = MapGraph[Int]()
  g.addVertex(1)
  g.addVertex(2)
  g.addVertex(3)
  g.addVertex(4)
  g.addVertex(5)
  g.addVertex(6)

  g.addEdge(1, 2)
  g.addEdge(1, 3)
  g.addEdge(2, 3)
  g.addEdge(Edge(3, 4))
  g.addEdge(4, 5)
  g.addEdge(5, 1)


  println(g.successors(3))
  println(g.vertices)
  println(g.edges)
  val traversal = g.breadthFirstTraversal(1)
  println(traversal.pathTo(4))
  println()

  val wg = MapWeightedGraph[Int, Double]()
  wg.addVertex(1)
  wg.addVertex(2)
  wg.addVertex(3)
  wg.addVertex(4)
  wg.addVertex(5)
  wg.addVertex(6)


  wg.addEdge(1, 2, 10)
  wg.addEdge(2, 3, 20)
  wg.addEdge(WeightedEdge(3, 4, 30))

  println(wg.successors(3))
  println(wg.incidentsFrom(3))
  println(wg.vertices)
  println(wg.edges)
}