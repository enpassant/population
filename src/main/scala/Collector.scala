/**
 *
 */
import akka.actor.Actor

/**
 * @author kalman
 *
 */
class Collector(val size: Int) extends Actor {

  var chromosomas: OrderedArrayBuffer[Chromosoma] = null

  val algorithm = context.actorFor("/user/population")
  
  def receive = {

    case Run =>
      chromosomas = new OrderedArrayBuffer[Chromosoma]

    case chromosoma: Chromosoma =>
      chromosomas.add(chromosoma)
      if (chromosomas.length >= size) {
        algorithm ! Change(chromosomas)
      }
  }
}