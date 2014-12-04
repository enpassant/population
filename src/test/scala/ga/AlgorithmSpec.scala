/**
 * Test actors
 */
package ga

import org.scalatest.{ BeforeAndAfterAll, FlatSpecLike, Matchers }
import akka.actor.{ Actor, Props, ActorSystem }
import akka.testkit.{ ImplicitSender, TestKit, TestActorRef }
import scala.concurrent.duration._
import samples.RandomNumbers

/**
 * @author kalman
 *
 */
class AlgorithmSpec(_system: ActorSystem) extends TestKit(_system) 
  with ImplicitSender
  with Matchers
  with FlatSpecLike
  with BeforeAndAfterAll
{

  def this() = this(ActorSystem("AlgorithmSpec"))

  override def afterAll: Unit = {
    system.shutdown()
    system.awaitTermination(10.seconds)
  }

  "Collector actor" should "be able to change algorithm's chromosomas" in {
    val collector = TestActorRef(Props(new Collector(2)))
    collector ! Run
    collector ! Add(self, Chromosoma(IndexedSeq(5), 0.0, None))
    collector ! Add(self, Chromosoma(IndexedSeq(5), 0.0, None))
    expectMsgPF() {
      case Change(chromosomas) => true
    }
  }
  
}