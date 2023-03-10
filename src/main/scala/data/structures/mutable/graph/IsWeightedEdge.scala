package data.structures.mutable.graph

/**
 * Interface for representing weighted edges.
 *
 * @tparam V type of vertices in edge.
 * @tparam W type of weights.
 * @author Pepe Gallardo
 */
trait IsWeightedEdge[+V, +W] extends IsEdge[V] with IsWeighted[W]

object IsWeightedEdge {
  def unapply[V, W](weightedEdge: IsWeightedEdge[V, W]): (V, V, W) =
    (weightedEdge.vertex1, weightedEdge.vertex2, weightedEdge.weight)
}
