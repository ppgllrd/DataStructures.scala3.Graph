package graph

/**
 * A unweighted directed edge.
 *
 * @param source      source vertex of unweighted directed edge.
 * @param destination destination vertex of unweighted directed edge.
 * @tparam V type of vertices in unweighted directed edge.
 * @author Pepe Gallardo
 */
class DirectedEdge[V](val source: V, val destination: V) {
  override def equals(other: Any): Boolean = other match {
    case that: DirectedEdge[?] =>
      (that canEqual this) && (source == that.source) && (destination == that.destination)
    case _ => false
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[DirectedEdge[?]]

  override def hashCode(): Int = {
    source.hashCode() + 31 * destination.hashCode()
  }

  override def toString: String = s"${getClass.getSimpleName}($source, $destination)"
}

object DirectedEdge {
  /**
   * Constructs a unweighted directed edge.
   *
   * @param source      source vertex of unweighted directed edge.
   * @param destination destination vertex of unweighted directed edge.
   * @tparam V type of vertices in unweighted directed edge.
   * @return a unweighted directed edge with given source and destination.
   */
  def apply[V](source: V, destination: V): DirectedEdge[V] = new DirectedEdge[V](source, destination)

  def unapply[V](directedEdge: DirectedEdge[V]): Option[(V, V)] = Some(directedEdge.source, directedEdge.destination)
}