package graph

import data.structures.mutable.graph.{DirectedEdge, Edge, MapDirectedGraph, MapGraph, MapWeightedGraph, WeightedEdge}

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
  println(g.edges.toList.sorted)
  println(g.incidentsFrom(3))
  println(g.incidentsTo(3))
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
  println()


  val dg = MapDirectedGraph[Int]()
  dg.addVertex(1)
  dg.addVertex(2)
  dg.addVertex(3)
  dg.addVertex(4)
  dg.addVertex(5)
  dg.addVertex(6)

  dg.addEdge(1, 2)
  dg.addEdge(1, 3)
  dg.addEdge(2, 3)
  dg.addEdge(DirectedEdge(3, 4))
  dg.addEdge(4, 5)
  dg.addEdge(5, 1)

  println(dg.successors(1))
  println(dg.vertices)
  println(dg.edges)
  println(dg.incidentsFrom(3))
  println(dg.incidentsTo(3))
  val traversalDg = dg.breadthFirstTraversal(1)
  println(traversalDg.pathTo(5))
  println()
}