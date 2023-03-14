package data.structures.mutable.connectedComponents

import data.structures.mutable.graph.*

import scala.collection.*


object ConnectedComponents {
  def apply[V](undirectedUnweightedGraph: UndirectedUnweightedGraph[V]): ConnectedComponents[V] = {
    new ConnectedComponents(undirectedUnweightedGraph) {
      override protected def buildComponent(vertices: Iterable[V]): UndirectedGraph[V] = {
        val component: UndirectedUnweightedGraph[V] = MapGraph[V]()

        for (vertex <- vertices) {
          component.addVertex(vertex)
        }
        for (vertex <- vertices) {
          for (edge <- undirectedUnweightedGraph.incidentsFrom(vertex)) {
            component.addEdge(edge)
          }
        }
        component
      }
    }
  }

  def apply[V, W](undirectedWeightedGraph: UndirectedWeightedGraph[V, W]): ConnectedComponents[V] = {
    new ConnectedComponents(undirectedWeightedGraph) {
      override protected def buildComponent(vertices: Iterable[V]): UndirectedGraph[V] = {
        val component: UndirectedWeightedGraph[V, W] = MapWeightedGraph[V, W]()

        for (vertex <- vertices) {
          component.addVertex(vertex)
        }
        for (vertex <- vertices) {
          for (edge <- undirectedWeightedGraph.incidentsFrom(vertex)) {
            component.addEdge(edge)
          }
        }
        component
      }
    }
  }
}

abstract class ConnectedComponents[V](undirectedGraph: UndirectedGraph[V]) {
  private var connectedComponents = List[UndirectedGraph[V]]()
  run()

  def components: List[UndirectedGraph[V]] = connectedComponents

  protected def buildComponent(vertices: Iterable[V])
  : UndirectedGraph[V]

  protected def run(): Unit = {
    val vertices = mutable.Set.from(undirectedGraph.vertices)

    while (vertices.nonEmpty) {
      val source = vertices.head
      val dft = undirectedGraph.depthFirstTraversal(source)

      val vs = vertices.filter(dft.isReachable)
      connectedComponents ::= buildComponent(vs)

      vertices --= vs
    }
  }
}

