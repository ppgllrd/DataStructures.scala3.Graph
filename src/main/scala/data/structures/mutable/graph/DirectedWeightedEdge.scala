package data.structures.mutable.graph

/**
 * A weighted directed edge.
 *
 * @param source      source vertex of the edge.
 * @param destination destination vertex of the edge.
 * @param weight      weight of the edge.
 * @tparam V type of vertices in edge.
 * @tparam W type of weights in edge.
 * @author Pepe Gallardo
 */
class DirectedWeightedEdge[V, W](source: V, destination: V, val weight: W) extends DirectedEdge[V](source,
  destination) {
  override def equals(other: Any): Boolean = other match {
    case that: DirectedWeightedEdge[?, ?] =>
      (that canEqual this) &&
        (source == that.source) && (destination == that.destination) && (weight == that.weight)
    case _ => false
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[DirectedWeightedEdge[?, ?]]

  override def hashCode(): Int = {
    source.hashCode() + 31 * (destination.hashCode() + 31 * (if (weight == null) 0 else weight.hashCode()))
  }

  override def toString: String = s"${getClass.getSimpleName}($source, $destination, $weight)"
}

object DirectedWeightedEdge {
  /**
   * Constructs a weighted directed edge.
   *
   * @param source      source vertex of the edge.
   * @param destination destination vertex of the edge.
   * @param weight      weight of the edge.
   * @tparam V type of vertices in edge.
   * @tparam W type of weights in edge.
   * @return a weighted directed edge.
   */
  def apply[V, W](source: V, destination: V, weight: W): DirectedWeightedEdge[V, W] =
    new DirectedWeightedEdge[V, W](source, destination, weight)

  def unapply[V, W](directedWeightedEdge: DirectedWeightedEdge[V, W]): (V, V, W) =
    (directedWeightedEdge.source, directedWeightedEdge.destination, directedWeightedEdge.weight)
}
