package data.structures.mutable.graph

/**
 * An unweighted undirected edge.
 *
 * @param vertex1 one endpoint of the edge.
 * @param vertex2 another endpoint of the edge.
 * @tparam V type of vertices in edge.
 * @author Pepe Gallardo
 */
class Edge[V](val vertex1: V, val vertex2: V) {
  override def equals(other: Any): Boolean = other match {
    case that: Edge[?] =>
      (that canEqual this) &&
        ((vertex1 == that.vertex1 && vertex2 == that.vertex2) ||
          (vertex1 == that.vertex2 && vertex2 == that.vertex1))
    case _ => false
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[Edge[?]]

  override def hashCode(): Int = {
    vertex1.hashCode() + vertex2.hashCode()
  }

  override def toString: String = s"${getClass.getSimpleName}($vertex1, $vertex2)"
}

object Edge {
  /** Constructs an unweighted undirected edge.
   *
   * @param vertex1 one endpoint of the edge.
   * @param vertex2 another endpoint of the edge.
   * @tparam V type of vertices in edge.
   * @return an unweighted undirected edge with given endpoints.
   */
  def apply[V](vertex1: V, vertex2: V): Edge[V] = new Edge[V](vertex1, vertex2)

  def unapply[V](edge: Edge[V]): Option[(V, V)] = Some(edge.vertex1, edge.vertex2)
}