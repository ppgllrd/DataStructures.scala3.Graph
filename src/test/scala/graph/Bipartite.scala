package graph

import data.structures.mutable.graph.MapGraph
import data.structures.mutable.graph.bipartiteness.Bipartiteness

@main def bipartite(): Unit = {

  def completeBipartite(n: Int, m: Int): MapGraph[Int] = {
    val g = new MapGraph[Int]
    for (v <- 0 until n + m) {
      g.addVertex(v)
    }
    for (v <- 0 until n) {
      for (w <- 0 until m) {
        g.addEdge(v, n + w)
      }
    }
    g
  }

  val bipartite = Bipartiteness(completeBipartite(20, 30))
  println(bipartite.isBipartite)
  println()


  val g = MapGraph[Int]()
  g.addVertex(0)
  g.addVertex(1)
  g.addVertex(2)
  g.addVertex(3)
  g.addVertex(4)

  g.addEdge(0, 3)
  g.addEdge(0, 4)
  g.addEdge(1, 3)
  g.addEdge(1, 4)
  g.addEdge(2, 3)

  println(Bipartiteness(g).isBipartite)

  g.addEdge(3, 4)
  println(Bipartiteness(g).isBipartite)

}
