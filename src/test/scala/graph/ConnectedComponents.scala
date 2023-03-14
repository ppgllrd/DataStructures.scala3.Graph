package graph

import data.structures.mutable.connectedComponents.ConnectedComponents
import data.structures.mutable.graph.{MapGraph, MapWeightedGraph}


@main def testConnectedComponents(): Unit = {
  val g = MapGraph[Int]()

  (1 to 10).foreach(g.addVertex)

  g.addEdge(1, 2)
  g.addEdge(2, 3)

  g.addEdge(4, 5)

  g.addEdge(6, 7)
  g.addEdge(7, 8)
  g.addEdge(8, 9)

  val cc = ConnectedComponents(g)

  cc.components.foreach(g => {
    println(g.vertices)
    println(g.edges)
  })

  println()

  val wg = MapWeightedGraph[Int, Double]()

  (1 to 10).foreach(wg.addVertex)

  wg.addEdge(1, 2, 12.0)
  wg.addEdge(2, 3, 23.0)

  wg.addEdge(4, 5, 45.0)

  wg.addEdge(6, 7, 67.0)
  wg.addEdge(7, 8, 78.0)
  wg.addEdge(8, 9, 89.0)

  val ccWg = ConnectedComponents(wg)

  ccWg.components.foreach(wg => {
    println(wg.vertices)
    println(wg.edges)
  })
}