package graph

/**
 * A pair consisting of a vertex and a weight.
 *
 * @param successor vertex in pair.
 * @param weight    weight in pair.
 * @tparam V type of vertex.
 * @tparam W type of weight.
 */
private[graph] case class Pair[V, W](successor: V, weight: W) {
  // only successor component is used for determining if two Pairs are equal
  override def equals(other: Any): Boolean = other match {
    case that: Pair[?, ?] =>
      (that canEqual this) && successor == successor
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Pair[?, ?]]

  // as equals only depends on successor so has to do hashCode
  override def hashCode(): Int = {
    successor.hashCode()
  }

  override def toString: String = s"${getClass.getSimpleName}($successor, $weight)"
}
