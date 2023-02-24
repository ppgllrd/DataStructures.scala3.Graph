package data.structures.mutable.graph

/**
 * A class for representing exceptions related to graphs.
 *
 * @param message the detail message.
 * @param cause   the cause.
 */
final case class GraphException(private val message: String = "", private val cause: Throwable = None.orNull)
  extends Exception(message, cause)
