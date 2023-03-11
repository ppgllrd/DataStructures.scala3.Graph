package data.structures.mutable.graph

/**
 * Interface for representing an edge.
 *
 * @tparam V type of vertices in edge.
 * @author Pepe Gallardo          
 */
trait IsEdge[+V] {
  def vertex1: V

  def vertex2: V
}

object IsEdge {
  def unapply[V](edge: IsEdge[V]): (V, V) =
    (edge.vertex1, edge.vertex2)
}