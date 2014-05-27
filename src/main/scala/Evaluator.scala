/**
 *
 */
import akka.actor.Actor
import scala.util.Random

/**
 * @author kalman
 *
 */
class Evaluator(val evaluate: Chromosoma => Chromosoma) extends Actor {
  val population = context.actorFor("akka://population/user/population")

  def receive = {
    case chromosoma: Chromosoma =>
      population ! evaluate(chromosoma)

    case Show => population ! Show
  }
}