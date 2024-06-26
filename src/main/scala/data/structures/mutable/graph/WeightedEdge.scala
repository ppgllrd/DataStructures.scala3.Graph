package data.structures.mutable.graph

/**
 * A weighted undirected edge.
 *
 * @param vertex1 one endpoint of the edge.
 * @param vertex2 another endpoint of the edge.
 * @param weight  weight of the edge.
 * @tparam V type of vertices in edge.
 * @tparam W type of weights in edge.
 * @author Pepe Gallardo
 */
class WeightedEdge[+V, +W](override val vertex1: V, override val vertex2: V, val weight: W)
  extends Edge[V](vertex1, vertex2) with IsWeightedEdge[V, W] {
  override def equals(other: Any): Boolean = other match {
    case that: WeightedEdge[?, ?] =>
      (that canEqual this) &&
        (weight == that.weight) &&
        ((vertex1 == that.vertex1 && vertex2 == that.vertex2) ||
          (vertex1 == that.vertex2 && vertex2 == that.vertex1))
    case _ => false
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[WeightedEdge[?, ?]]

  override def hashCode(): Int = {
    vertex1.hashCode() + vertex2.hashCode() + 31 * (if (weight == null) 0 else weight.hashCode())
  }

  override def toString: String = s"${getClass.getSimpleName}($vertex1, $vertex2, $weight)"
}

object WeightedEdge {
  /**
   * Constructs a weighted undirected edge.
   *
   * @param vertex1 one endpoint of the edge.
   * @param vertex2 another endpoint of the edge.
   * @param weight  weight of the edge.
   * @tparam V type of vertices in edge.
   * @tparam W type of weights in edge.
   * @return a weighted undirected edge with given endpoints and weight.
   */
  def apply[V, W](vertex1: V, vertex2: V, weight: W): WeightedEdge[V, W] =
    new WeightedEdge[V, W](vertex1, vertex2, weight)

  def unapply[V, W](weightedEdge: WeightedEdge[V, W]): (V, V, W) =
    (weightedEdge.vertex1, weightedEdge.vertex2, weightedEdge.weight)

  given weightedEdgeOrdering[V, W](using Ordering[V], Ordering[W]): Ordering[WeightedEdge[V, W]] =
    Ordering.by[WeightedEdge[V, W], V](_.vertex1) orElse
      Ordering.by[WeightedEdge[V, W], V](_.vertex2) orElse
      Ordering.by[WeightedEdge[V, W], W](_.weight)
}


