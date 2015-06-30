package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import akka.event.LoggingReceive
import scala.concurrent.duration._
import scala.language.postfixOps

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  case class Unregister()
  case class ScheduleTick()

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  override def preStart() = {
    context.system.scheduler.schedule(100 milliseconds, 100 milliseconds) {
      self ! ScheduleTick
    }
  }

  def send(req: Replicate, seq: Long) = {
    replica ! Snapshot(req.key, req.valueOption, seq)
  }

  def resend() = {
    acks.foreach {
      case (seq, (_, req)) => send(req, seq)
    }
  }

  /* TODO Behavior for the Replicator. */
  def receive: Receive = LoggingReceive {
    case ScheduleTick => resend
    case Replicate(key, valueOption, id) => {
      val seq = nextSeq
      val req = new Replicate(key, valueOption, id)
      acks += seq -> (sender, req)
      send(req, seq)
    }
    case SnapshotAck(key, seq) => {
      acks get seq foreach { case (user, r) => {
        user ! Replicated(key, r.id)
        acks -= seq
      }}
    }
    case Unregister => {
      acks foreach { case (key, value) => value match {
        case (user, Replicate(key, valueOption, id)) => {
          user ! Replicated(key, id)
        }
      }}
      context.stop(self)
    }
  }

}

