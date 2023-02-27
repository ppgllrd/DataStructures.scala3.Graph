package graph

import data.structures.mutable.graph.MapWeightedGraph
import data.structures.mutable.graph.minimumSpanningTrees.Kruskal

object KruskalTest extends App {
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

  val kruskal = new Kruskal(wg)
  println(kruskal.minimumSpanningTree.edges)
}
