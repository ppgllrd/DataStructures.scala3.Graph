package data.structures.mutable.graph

/**
 * Interface for representing weighted entities.
 *
 * @tparam W type of weights.
 * @author Pepe Gallardo          
 */
trait IsWeighted[+W] {
  def weight: W
}
