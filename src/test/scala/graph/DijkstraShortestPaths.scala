package graph

import graph.shortestPaths.Dijkstra

object DijkstraShortestPaths extends App {
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
  val dijkstra = new Dijkstra(wg, source)
  for (vertex <- wg.vertices)
    println(s"Cost of shortest path from $source to $vertex is ${dijkstra.lowestCostTo(vertex)}")

  println()
  for (vertex <- wg.vertices)
    println(s"Shortest path from $source to $vertex is ${dijkstra.shortestPathTo(vertex)}")
}
