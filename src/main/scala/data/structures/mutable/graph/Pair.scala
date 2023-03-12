package data.structures.mutable.graph

/**
 * A pair consisting of a vertex and a weight.
 *
 * @param vertex vertex in pair.
 * @param weight weight in pair.
 * @tparam V type of vertex.
 * @tparam W type of weight.
 */
private[graph] case class Pair[V, W](vertex: V, weight: W) {
  def this(vertex: V) =
    this(vertex, null.asInstanceOf[W])

  // only vertex component is used for determining if two Pairs are equal when
  // any of compared weights are null
  override def equals(other: Any): Boolean = other match {
    case that: Pair[?, ?] =>
      (that canEqual this) && vertex == that.vertex &&
        (weight == null || that.weight == null || weight == that.weight)
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Pair[?, ?]]

  // as equals only depends on vertex so has to do hashCode
  override def hashCode(): Int = {
    vertex.hashCode()
  }

  override def toString: String = s"${getClass.getSimpleName}($vertex, $weight)"
}

object Pair {
  def apply[V, W](vertex: V): Pair[V, W] = new Pair(vertex)
}