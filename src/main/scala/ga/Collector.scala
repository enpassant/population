/**
 *
 */
package ga

import akka.actor.Actor
import akka.actor.actorRef2Scala
import akka.actor.ActorRef

/**
 * @author kalman
 *
 */
class Collector(val size: Int) extends Actor {

  var chromosomas: OrderedArrayBuffer[Chromosoma] = null

  def receive = {

    case Run =>
      chromosomas = new OrderedArrayBuffer[Chromosoma]

    case Add(algorithm: ActorRef, chromosoma: Chromosoma) =>
      chromosomas.add(chromosoma)
      if (chromosomas.length >= size) {
        algorithm ! Change(chromosomas)
      }
  }
}