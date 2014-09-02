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

  "Collector actor" should "not run" in {
    val collector = TestActorRef(Props(new Collector(2)))
    collector.underlyingActor.asInstanceOf[Collector].chromosomas should be(null)
  }
  
  it should "be able to run" in {
    val collector = TestActorRef(Props(new Collector(2)))
    collector ! Run
    collector.underlyingActor.asInstanceOf[Collector].chromosomas should not be(null)
  }
  
  it should "be able to add chromosoma" in {
    val collector = TestActorRef(Props(new Collector(2)))
    collector ! Run
    collector ! Add(self, new Chromosoma(5))
    collector.underlyingActor.asInstanceOf[Collector].chromosomas.length should be(1)
  }
  
  it should "be able to change algorithm's chromosomas" in {
    val collector = TestActorRef(Props(new Collector(2)))
    collector ! Run
    collector ! Add(self, new Chromosoma(5))
    collector ! Add(self, new Chromosoma(5))
    expectMsgPF() {
      case Change(chromosomas) => true
    }
  }
  
}