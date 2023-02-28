package data.structures.mutable.heap

/**
 * A locator can be used for fast access to elements stored in heap or map. Cannot be used once
 * a new insertion in the heap has been performed, once an update of the map without using the locator has been
 * performed or once a new locator for other object hash been reclaimed.
 *
 * @author Pepe Gallardo
 */
class Locator private[heap](val index: Int) extends AnyVal