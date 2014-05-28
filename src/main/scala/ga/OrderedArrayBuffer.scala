/**
 *
 */
package ga

import scala.collection.mutable.ArrayBuffer

/**
 * @author kalman
 *
 */
class OrderedArrayBuffer[A <: Ordered[A]] extends ArrayBuffer[A] {
	def add(elem: A) = {
	  def getPos(): Int = {
	    for (i <- 0 until length) if (apply(i) <= elem) return i
	    return length
	  }
	  insert(getPos, elem)
	}
}