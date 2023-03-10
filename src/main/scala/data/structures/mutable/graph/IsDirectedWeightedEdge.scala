package data.structures.mutable.graph

/**
 * Interface for representing a directed weighted edge.
 *
 * @tparam V type of vertices in edge.
 * @tparam W type of weights in edge.
 * @author Pepe Gallardo          
 */
trait IsDirectedWeightedEdge[+V, +W] extends IsDirectedEdge[V] with IsWeighted[W]
