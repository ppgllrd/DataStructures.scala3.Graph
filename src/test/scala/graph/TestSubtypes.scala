package graph

import data.structures.mutable.graph.{Edge, Graph, MapGraph, MapWeightedGraph, WeightedEdge, WeightedGraph}

import java.nio.DoubleBuffer

@main def TestSubtypes(): Unit = {
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


object Version1 extends App {
  trait EDGE[+V] {
    def v: V
  }

  trait WEDGE[+V, W] extends EDGE[V] {
    def w: W
  }

  class E[+V](val _v: V) extends EDGE[V] {
    def v: V = _v
  }

  class WE[+V, W](override val _v: V, val _w: W) extends E[V](_v) with WEDGE[V, W] {
    def w: W = _w
  }

  class G[V, +E[_]] {
    def f[SE[T] >: E[T] <: EDGE[T]](e: SE[V]): Unit = {
      e.v
    }

    def es: List[E[V]] = List.empty
  }

  class WG[V, W, +WE[_, _]] extends G[V, [X] =>> WE[X, W]] {
    def f[SWE[T] >: WE[T, W] <: WEDGE[T, W]](e: SWE[V]): Unit = {
      e.w
    }

    override def es: List[WE[V, W]] = List.empty
  }

  val e = E[Int](1)
  val we = WE[Int, Double](2, 30.0)

  val g = new G[Int, E]
  val wg = new WG[Int, Double, WE]
  val g2: G[Int, E] = wg // .asInstanceOf[A[Int, E]]

  g.f(e)
  g.f(we)

  g2.f(e)
  g2.f(we)

  wg.f(e)
  wg.f(we)
}

object Version2 extends App {
  class EDGE[+V](val _v: V) {
    def v: V = _v
  }

  class WEDGE[+V, W](override val _v: V, val _w: W) extends EDGE[V](_v) {
    def w: W = _w
  }

  class G[V] {
    def f(e: EDGE[V]): Unit = {
      e.v
    }

    def es: List[EDGE[V]] = List.empty
  }

  class WG[V, W] extends G[V] {
    def f(e: WEDGE[V, W]): Unit = { // cannot override
      e.w
    }

    override def es: List[WEDGE[V, W]] = List.empty
  }

  val e = EDGE[Int](1)
  val we = WEDGE[Int, Double](2, 30.0)

  val g = new G[Int]
  val wg = new WG[Int, Double]
  val g2: G[Int] = wg // .asInstanceOf[A[Int, E]]

  g.f(e)
  g.f(we)

  g2.f(e)
  g2.f(we)

  wg.f(e)
  wg.f(we)

  wg.es
}