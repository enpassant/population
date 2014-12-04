/**
 *
 */
package ga

import akka.actor.Actor
import akka.actor.actorRef2Scala
import akka.actor.ActorRef
import scala.concurrent.duration._
import akka.actor.ReceiveTimeout

/**
 * @author kalman
 *
 */
class Collector(val size: Int) extends Actor {

  context.setReceiveTimeout(5 seconds)

  def receive = active(new OrderedArrayBuffer[Chromosoma])
    
  def active(chromosomas: OrderedArrayBuffer[Chromosoma]): Receive = {

    case Run =>
      context become active(new OrderedArrayBuffer[Chromosoma])

    case Add(algorithm: ActorRef, chromosoma: Chromosoma) =>
      chromosomas.add(chromosoma)
      if (chromosomas.length >= size) {
        algorithm ! Change(chromosomas)
      }
    
    case ReceiveTimeout ⇒ context.actorSelection("..") ! Change(chromosomas)
  }
}