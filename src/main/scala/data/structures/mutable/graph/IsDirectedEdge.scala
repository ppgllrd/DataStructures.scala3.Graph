package data.structures.mutable.graph

/**
 * Interface for representing a directed edge.
 *
 * @tparam V type of vertices in directed edge.
 * @author Pepe Gallardo         
 */
trait IsDirectedEdge[+V] {
  def source: V

  def destination: V
}

object IsDirectedEdge {
  def unapply[V, W](directedEdge: IsDirectedEdge[V]): (V, V) =
    (directedEdge.source, directedEdge.destination)
}