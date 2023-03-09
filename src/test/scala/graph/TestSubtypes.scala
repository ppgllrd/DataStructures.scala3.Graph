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

object Version3 extends App {

  trait IsWeightedEdge[+W] {
    def w: W
  }

  trait IsEdge[+V] {
    def v: V
  }

  trait IsDiEdge[+V] {
    def src: V
  }

  class EDGE[+V](protected val _v: V) extends IsEdge[V] {
    def v: V = _v
  }

  class WEDGE[+V, +W](override protected val _v: V, protected val _w: W) extends EDGE[V](_v) with IsWeightedEdge[W] {
    def w: W = _w
  }

  class DiEDGE[+V](protected val _src: V) extends IsDiEdge[V] {
    def src: V = _src
  }

  class DiWEDGE[+V, +W](override protected val _src: V, protected val _w: W) extends DiEDGE[V](_src) with IsWeightedEdge[W] {
    override def src: V = _src
    override def w: W = _w
  }


  trait IsGraph[V, E[_]] {
    def addV(v: V): Unit
    def vs: Set[V]

    def addE(e: E[V]): Unit
    def es: Set[E[V]]
  }


  trait IsWeightedGraph[V, W, E[+_] <: IsWeightedEdge[W]] extends IsGraph[V, E]

  // trait TDG[V, E[_] <: TDE[V]] extends GL[V, E]

  trait IsUndirectedGraph[V] extends IsGraph[V, EDGE]

  trait IsUndirectedWeightedGraph[V, W] extends IsWeightedGraph[V, W, [X] =>> WEDGE[X, W]]

  trait IsDirectedGraph[V] extends IsGraph[V, DiEDGE]

  trait IsDirectedWeightedGraph[V, W] extends IsDirectedGraph[V] with IsWeightedGraph[V, W, [X] =>> DiWEDGE[X, W]]


  class GRAPH[V] extends IsUndirectedGraph[V] {
    override def es: Set[EDGE[V]] = Set.empty

    override def addV(v: V): Unit = {
    }

    override def vs: Set[V] = Set.empty

    override def addE(e: EDGE[V]): Unit = {
      e.v
    }
  }

  class WEIGHTEDGRAPH[V, W] extends IsUndirectedWeightedGraph[V, W] {
    override def es: Set[WEDGE[V, W]] = Set.empty

    override def addV(v: V): Unit = ???

    override def vs: Set[V] = ???

    override def addE(e: WEDGE[V, W]): Unit = {
      (e.v, e.w)
    }
  }



  val e = EDGE[Int](1)
  val we = WEDGE[Int, Double](2, 30.0)

  val g = new GRAPH[Int]
  val wg = new WEIGHTEDGRAPH[Int, Double]




  def f1[V, W](g: IsGraph[V, EDGE] | IsWeightedGraph[V, W, [X] =>> WEDGE[X, W]]): Unit = {
    val e: IsEdge[V] = g.es.head
    println(e.v)

  }

  // f1(g.asInstanceOf[IsGraph[Int, IsEdge]])
  // f1(wg.asInstanceOf[IsGraph[Int, IsEdge]])

  f1(g)
  f1(wg)


  // val g2: G[Int] = wg // .asInstanceOf[A[Int, E]]

}



object Version4 extends App {
  import scala.collection._

  trait IsWeighted[+W] {
    def weight: W
  }

  trait IsEdge[+V] {
    def vertex1: V
    def vertex2: V
  }

  trait IsDirectedEdge[+V] {
    def source: V
    def destination: V
  }

  trait IsWeightedEdge[+V, +W] extends IsEdge[V] with IsWeighted[W]

  trait IsDirectedWeightedEdge[+V, +W] extends IsDirectedEdge[V] with IsWeighted[W]

  class Edge[+V](protected val _v1: V, protected val _v2: V) extends IsEdge[V] {
    override def vertex1: V = _v1
    override def vertex2: V = _v2
  }

  class WeightedEdge[+V, +W](override protected val _v1: V, override protected val _v2: V, protected val _w: W)
    extends Edge[V](_v1, _v2) with IsWeightedEdge[V, W] {
    override def weight: W = _w
  }

  class DirectedEdge[+V](protected val _src: V, protected val _dst: V) extends IsEdge[V] with IsDirectedEdge[V] {
    override def vertex1: V = _src
    override def vertex2: V = _dst
    override def source: V = _src
    override def destination: V = _dst
  }

  class DirectedWeightedEdge[+V, +W](override protected val _src: V, override protected val _dst: V, protected val _w: W)
    extends DirectedEdge[V](_src, _dst) with IsWeightedEdge[V, W] with IsDirectedWeightedEdge[V, W] {
    override def source: V = _src
    override def weight: W = _w
  }

  trait Graph[V, +E[_]] {
    def addVertex(vertex: V): Boolean
    def deleteVertex(vertex: V): Boolean
    def containsVertex(vertex: V): Boolean

    def vertices: Set[V]
    def order: Int

    def edges[Edge[X] >: E[X]]: Set[Edge[V]]
    def size: Int

    def deleteEdge(vertex1: V, vertex2: V): Boolean
    def containsEdge(vertex1: V, vertex2: V): Boolean

    def successors(v: V): immutable.Set[V]
    def edgesFrom[Edge[X] >: E[X]](vertex: V): immutable.Set[Edge[V]]
    def degree(vertex: V): Int
  }

  trait UndirectedGraph[V] extends Graph[V, Edge]

  trait DirectedGraph[V] extends Graph[V, DirectedEdge] {
    def predecessors(v: V): immutable.Set[V]
    def edgesTo[Edge[X] >: DirectedEdge[X]](v: V): immutable.Set[Edge[V]]
    def indegree(vertex: V): Int
    def outdegree(vertex: V): Int
  }

  // trait WeightedGraph[V, W, +E[X, Y] <: IsEdge[X] with IsWeighted[Y]] extends Graph[V, [X] =>> E[X, W]]

  trait WeightedGraph[V, W] extends Graph[V, [X] =>> IsWeightedEdge[X, W]] {
    def addEdge(vertex1: V, vertex2: V, weight: W): Boolean
    def deleteEdge(vertex1: V, vertex2: V, weight: W): Boolean
    def containsEdge(vertex1: V, vertex2: V, weight: W): Boolean
    def weightOfEdge(vertex1: V, vertex2: V): Option[W]
    def successorsAndWeights(vertex: V): immutable.Set[(V, W)]
  }

  trait UnweightedGraph[V] extends Graph[V, IsEdge] {
    def addEdge(vertex1: V, vertex2: V): Boolean
    def deleteEdge(vertex1: V, vertex2: V): Boolean
  }

  trait UndirectedWeightedGraph[V, W] extends UndirectedGraph[V] with Graph[V, [X] =>> WeightedEdge[X, W]]
    with WeightedGraph[V, W] {
    def addEdge(weightedEdge: WeightedEdge[V, W]): Unit
    def containsEdge(weightedEdge: WeightedEdge[V, W]): Boolean
    def deleteEdge(weightedEdge: WeightedEdge[V, W]): Unit
  }

  trait DirectedWeightedGraph[V, W] extends DirectedGraph[V] with Graph[V, [X] =>> DirectedWeightedEdge[X, W]]
    with WeightedGraph[V, W] {
    def addEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Unit
    def containsEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Boolean
    def deleteEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Unit
  }

  trait UndirectedUnweightedGraph[V] extends UndirectedGraph[V] with UnweightedGraph[V] {
    def addEdge(edge: Edge[V]): Unit
    def containsEdge(edge: Edge[V]): Boolean
    def deleteEdge(edge: Edge[V]): Unit
  }

  trait DirectedUnweightedGraph[V] extends DirectedGraph[V] with UnweightedGraph[V] {
    def addEdge(directedEdge: DirectedEdge[V]): Unit
    def containsEdge(directedEdge: DirectedEdge[V]): Boolean
    def deleteEdge(directedEdge: DirectedEdge[V]): Unit
  }

  class MapGraph[V] extends UndirectedUnweightedGraph[V] {
    private var xs =  List[Edge[V]]()

    override def addEdge(edge: Edge[V]): Unit = ???

    override def containsEdge(edge: Edge[V]): Boolean = ???

    override def deleteEdge(edge: Edge[V]): Unit = ???

    override def addEdge(vertex1: V, vertex2: V): Boolean = ???

    override def deleteEdge(vertex1: V, vertex2: V): Boolean = ???

    override def addVertex(vertex: V): Boolean = ???

    override def deleteVertex(vertex: V): Boolean = ???

    override def containsVertex(vertex: V): Boolean = ???

    override def vertices: Set[V] = ???

    override def order: Int = ???

    override def edges[E[X] >: Edge[X]]: Set[E[V]] = ???

    override def size: Int = ???

    override def containsEdge(vertex1: V, vertex2: V): Boolean = ???

    override def successors(v: V): immutable.Set[V] = ???

    override def edgesFrom[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] = ???

    override def degree(vertex: V): Int = ???
  }

  class MapWeightedGraph[V, W] extends UndirectedWeightedGraph[V, W] {
    private var xs =  List[WeightedEdge[V, W]]()

    override def addEdge(weightedEdge: WeightedEdge[V, W]): Unit = ???

    override def containsEdge(weightedEdge: WeightedEdge[V, W]): Boolean = ???

    override def deleteEdge(weightedEdge: WeightedEdge[V, W]): Unit = ???

    override def addEdge(vertex1: V, vertex2: V, weight: W): Boolean = ???

    override def deleteEdge(vertex1: V, vertex2: V, weight: W): Boolean = ???

    override def containsEdge(vertex1: V, vertex2: V, weight: W): Boolean = ???

    override def weightOfEdge(vertex1: V, vertex2: V): Option[W] = ???

    override def successorsAndWeights(vertex: V): immutable.Set[(V, W)] = ???

    override def addVertex(vertex: V): Boolean = ???

    override def deleteVertex(vertex: V): Boolean = ???

    override def containsVertex(vertex: V): Boolean = ???

    override def vertices: Set[V] = ???

    override def order: Int = ???

    override def edges[Edge[X] >: WeightedEdge[X, W]]: Set[Edge[V]] = ???

    override def size: Int = ???

    override def deleteEdge(vertex1: V, vertex2: V): Boolean = ???

    override def containsEdge(vertex1: V, vertex2: V): Boolean = ???

    override def successors(v: V): immutable.Set[V] = ???

    override def edgesFrom[Edge[X] >: WeightedEdge[X, W]](vertex: V): immutable.Set[Edge[V]] = ???

    override def degree(vertex: V): Int = ???
  }

  class MapDirectedGraph[V] extends DirectedUnweightedGraph[V] {
    override def addEdge(directedEdge: DirectedEdge[V]): Unit = ???

    override def containsEdge(directedEdge: DirectedEdge[V]): Boolean = ???

    override def deleteEdge(directedEdge: DirectedEdge[V]): Unit = ???

    override def addEdge(source: V, destination: V): Boolean = ???

    override def deleteEdge(source: V, destination: V): Boolean = ???

    override def predecessors(v: V): immutable.Set[V] = ???

    override def edgesTo[Edge[X] >: DirectedEdge[X]](v: V): immutable.Set[Edge[V]] = ???

    override def indegree(vertex: V): Int = ???

    override def outdegree(vertex: V): Int = ???

    override def addVertex(vertex: V): Boolean = ???

    override def deleteVertex(vertex: V): Boolean = ???

    override def containsVertex(vertex: V): Boolean = ???

    override def vertices: Set[V] = ???

    override def order: Int = ???

    override def edges[Edge[X] >: DirectedEdge[X]]: Set[Edge[V]] = ???

    override def size: Int = ???

    override def containsEdge(source: V, destination: V): Boolean = ???

    override def successors(v: V): immutable.Set[V] = ???

    override def edgesFrom[Edge[X] >: DirectedEdge[X]](vertex: V): immutable.Set[Edge[V]] = ???

    override def degree(vertex: V): Int = ???
  }

  class MapDirectedWeightedGraph[V, W] extends DirectedWeightedGraph[V, W] {
    override def addEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Unit = ???

    override def containsEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Boolean = ???

    override def deleteEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Unit = ???

    override def addEdge(source: V, destination: V, weight: W): Boolean = ???

    override def deleteEdge(source: V, destination: V, weight: W): Boolean = ???

    override def containsEdge(source: V, destination: V, weight: W): Boolean = ???

    override def weightOfEdge(source: V, destination: V): Option[W] = ???

    override def successorsAndWeights(vertex: V): immutable.Set[(V, W)] = ???

    override def predecessors(v: V): immutable.Set[V] = ???

    override def edgesTo[Edge[X] >: DirectedWeightedEdge[X, W]](v: V): immutable.Set[Edge[V]] = ???

    override def indegree(vertex: V): Int = ???

    override def outdegree(vertex: V): Int = ???

    override def addVertex(vertex: V): Boolean = ???

    override def deleteVertex(vertex: V): Boolean = ???

    override def containsVertex(vertex: V): Boolean = ???

    override def vertices: Set[V] = ???

    override def order: Int = ???

    override def edges[Edge[X] >: DirectedWeightedEdge[X, W]]: Set[Edge[V]] = ???

    override def size: Int = ???

    override def deleteEdge(source: V, destination: V): Boolean = ???

    override def containsEdge(source: V, destination: V): Boolean = ???

    override def successors(v: V): immutable.Set[V] = ???

    override def edgesFrom[Edge[X] >: DirectedWeightedEdge[X, W]](vertex: V): immutable.Set[Edge[V]] = ???

    override def degree(vertex: V): Int = ???
  }

  val e = new Edge[Int](1, 2)
  val we = new WeightedEdge[Int, Double](2, 3, 15.0)

  val wedge : IsEdge[Int] with IsWeighted[Double] = new WeightedEdge(1,2,3.0)


  val g = new MapGraph[Int]
  g.addEdge(new Edge(3, 4))
  g.addEdge(2, 7)

  val wg = new MapWeightedGraph[Int, Double]
  wg.addEdge(new WeightedEdge(1, 2, 10.0))
  wg.addEdge(1, 7, 15.3)

  val g2 : UndirectedGraph[Int] = wg

  println(wg.edges)
  println(g2.edges)


  def f1[V](g: UndirectedGraph[V]): Unit = {
    val e: Edge[V] = g.edges.head
    println(e.vertex1)
  }


  f1(g)
  f1(wg)

  /*
  def f2[V, W](g: WeightedGraph[V, W, IsWeightedEdge]): Unit = {
    println(g.edges.head.weight)
    println(g.edges.head.vertex1)

  }
  */

  def f2[V, W](g: WeightedGraph[V, W]): Unit = {
    val e = g.edges.head
    println(g.edges.head.weight)
    println(g.edges.head.vertex1)
  }

  val dwg = new MapDirectedWeightedGraph[Int, Double]
  dwg.addEdge(new DirectedWeightedEdge(1, 2, 5.6))
  dwg.addEdge(3, 4, 7.7)


  f2(dwg)
  f2(wg)


  def f3[V, W](g: DirectedGraph[V]): Unit = {
    println(g.edges.head.vertex1)
    println(g.edges.head.source)
  }

  val dg = new MapDirectedGraph[Int]
  dg.addEdge(new DirectedEdge(1, 2))
  dg.addEdge(1, 6)


  f3(dg)
  f3(dwg)



  def f4[V, W](g: DirectedWeightedGraph[V, W]): Unit = {
    println(g.edges.head.weight)
    println(g.edges.head.vertex1)
    println(g.edges.head.source)
  }

  f4(dwg)
}