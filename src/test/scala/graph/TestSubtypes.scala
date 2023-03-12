package graph

import data.structures.mutable.graph.*

import scala.collection.*


@main def TestSubtypes(): Unit = {
  val e = Edge[Int](1, 2)
  val we = WeightedEdge[Int, Double](2, 3, 15.0)

  val wedge: IsEdge[Int] with IsWeighted[Double] = WeightedEdge(1, 2, 3.0)

  val g = MapGraph[Int]()
  g.addVertex(2)
  g.addVertex(3)
  g.addVertex(4)
  g.addVertex(7)
  g.addEdge(Edge(3, 4))
  g.addEdge(2, 7)

  val wg = MapWeightedGraph[Int, Double]()
  wg.addVertex(1)
  wg.addVertex(2)
  wg.addVertex(7)
  wg.addEdge(WeightedEdge(1, 2, 10.0))
  wg.addEdge(1, 7, 15.3)

  val g2: UndirectedGraph[Int] = wg

  val dg = MapDirectedGraph[Int]()
  dg.addEdge(DirectedEdge(1, 2))
  dg.addEdge(1, 6)

  val dwg = MapDirectedWeightedGraph[Int, Double]()
  dwg.addEdge(DirectedWeightedEdge(1, 2, 5.6))
  dwg.addEdge(3, 4, 7.7)

  println(wg.edges)
  println(g2.edges)


  // more general. Any type of graph
  def f1[V](g: Graph[V, IsEdge]): Unit = {
    val e: IsEdge[V] = g.edges.head
    println(e.vertex1)

    val v = g.vertices.head
    val es: immutable.Set[IsEdge[V]] = g.incidentsFrom(v)
    println(es.head.vertex1)
  }

  f1(g)
  f1(wg)
  f1(dg)
  f1(dwg)


  // only undirected graphs
  def f2[V](g: UndirectedGraph[V]): Unit = {
    val e: IsEdge[V] = g.edges.head
    println(e.vertex1)

    val v = g.vertices.head
    val es: immutable.Set[IsEdge[V]] = g.incidentsFrom(v)
    println(es.head.vertex1)
  }

  f2(g)
  f2(wg)
  // error f2(dg)
  // error f2(dwg)


  // only weighted graphs
  def f3[V, W](g: WeightedGraph[V, W]): Unit = {
    val e: IsWeightedEdge[V, W] = g.edges.head
    println(e.weight)
    println(e.vertex1)

    val v = g.vertices.head
    val es: immutable.Set[IsWeightedEdge[V, W]] = g.incidentsFrom(v)
    println(es.head.weight)
  }

  f3(dwg)
  f3(wg)
  // error f3(g)
  // error f3(dg)


  // only directed graphs
  def f4[V](g: DirectedGraph[V]): Unit = {
    val e: IsDirectedEdge[V] = g.edges.head
    println(e.source)

    val v = g.vertices.head
    val es: immutable.Set[IsDirectedEdge[V]] = g.incidentsFrom(v)
    println(es.head.source)
  }

  f4(dg)
  f4(dwg)


  // only directed weighted graphs
  def f5[V, W](g: DirectedWeightedGraph[V, W]): Unit = {
    val e: IsDirectedWeightedEdge[V, W] = g.edges.head
    println(e.weight)
    println(e.source)

    val v = g.vertices.head
    // Set is not covariant !!!
    // val es: immutable.Set[IsDirectedWeightedEdge[V, W]] = g.incidentsFrom(v)
    val es: Iterable[IsDirectedWeightedEdge[V, W]] = g.incidentsFrom(v)
    val es2: Set[DirectedWeightedEdge[V, W]] = g.incidentsFrom(v)
    val e2: IsDirectedWeightedEdge[V, W] = g.incidentsFrom(v).head
    println(es.head.weight)
    println(es.head.source)
  }

  f5(dwg)
  // error f5(g)
  // error f5(wg)
  // error f5(dg)
}