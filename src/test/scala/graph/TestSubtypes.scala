package graph

import data.structures.mutable.graph.{Edge, Graph, MapGraph, MapWeightedGraph, WeightedEdge, WeightedGraph}

@main def TestSubtypes(): Unit =  {
  val wg: WeightedGraph[Int, Double, WeightedEdge] = MapWeightedGraph[Int, Double]()

  wg.addVertex(1)
  wg.addVertex(2)
  wg.addVertex(3)

  wg.addEdge(1, 2, 10.0)
  wg.addEdge(2, 3, 20.0)
  wg.addEdge(1, 3, 30.0)


  println(wg.edges)

  // todo why isn't working without asInstanceOf
  val g: Graph[Int, Edge] = wg.asInstanceOf[Graph[Int, Edge]]

  println(g.edges)

}
