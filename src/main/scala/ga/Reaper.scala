/**
 *
 */
package ga

import scala.collection.mutable.ArrayBuffer
import akka.actor.Terminated
import akka.actor.Actor
import akka.actor.ActorRef

/**
 * @author kalman
 *
 */

object Reaper {
  // Used by others to register an Actor for watching
  case class WatchMe(ref: ActorRef)
}

class Reaper extends Actor {
  import Reaper._

  // Keep track of what we're watching
  val watched = ArrayBuffer.empty[ActorRef]

  // It's the hook that's called when everything's dead
  def allSoulsReaped() = context.system.shutdown()

  // Watch and check for termination
  final def receive = {
    case WatchMe(ref) =>
      context.watch(ref)
      watched += ref
    case Terminated(ref) =>
      watched -= ref
      if (watched.isEmpty) allSoulsReaped()
  }
}
