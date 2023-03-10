package graph

import data.structures.mutable.graph.*

import scala.collection.*


@main def TestSubtypes(): Unit = {
  val e = new Edge[Int](1, 2)
  val we = new WeightedEdge[Int, Double](2, 3, 15.0)

  val wedge: IsEdge[Int] with IsWeighted[Double] = new WeightedEdge(1, 2, 3.0)

  val g = new MapGraph[Int]
  g.addEdge(new Edge(3, 4))
  g.addEdge(2, 7)

  val wg = new MapWeightedGraph[Int, Double]
  wg.addEdge(new WeightedEdge(1, 2, 10.0))
  wg.addEdge(1, 7, 15.3)

  val g2: UndirectedGraph[Int] = wg

  val dg = new MapDirectedGraph[Int]
  dg.addEdge(new DirectedEdge(1, 2))
  dg.addEdge(1, 6)

  val dwg = new MapDirectedWeightedGraph[Int, Double]
  dwg.addEdge(new DirectedWeightedEdge(1, 2, 5.6))
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