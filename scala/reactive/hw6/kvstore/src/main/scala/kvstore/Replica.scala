package kvstore

import akka.actor._
import akka.event.LoggingReceive
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import scala.concurrent.duration._
import akka.util.Timeout
import scala.language.postfixOps

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply
  case class ScheduleCheck(key: String, id: Long)

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

//  context.setReceiveTimeout(1 second) // the receive timeout is reset by every received message

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  var expectedSeq = 0L

  var persistent = context.actorOf(persistenceProps)
  var persistenceIdToClient = Map.empty[Long, (ActorRef, Persist)]

  override def preStart() : Unit= {
    arbiter ! Join
    context.system.scheduler.schedule(0 milliseconds, 100 milliseconds)(repersist)
  }

  def replicate(key: String, valueOption: Option[String], id: Long) = {
    secondaries.values.foreach { replicator => {
      BothAck.registerReplication(key, id, replicator, sender)
      replicator ! Replicate(key, valueOption, id)
    }}
    BothAck.registerPersistence(key, id, sender)
    persist(key, valueOption, id)
    context.system.scheduler.scheduleOnce(1 second, self, ScheduleCheck(key, id))
  }

  def persist(key: String, valueOption: Option[String], seq: Long) = {
    val p = Persist(key, valueOption, seq)
    persistenceIdToClient += (seq -> (sender, p))
    persistent ! p
  }

  def repersist() = {
    persistenceIdToClient.foreach {
      case (id, (_, p)) => persistent ! p
    }
  }

  def receive = LoggingReceive {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = LoggingReceive {
    case Arbiter.Replicas(replicas) => {
      val added = replicas -- secondaries.keySet - self
      val removed = secondaries.keySet -- replicas
      added.foreach { x => {
        val r = context.actorOf(Replicator.props(x))
        secondaries += x -> r
        kv foreach {
          case (k, v) => r ! Replicate(k, Some(v), -1)
        }
      }}
      removed foreach { x => {
        BothAck.removeReplicator(secondaries(x))
        context stop secondaries(x)
        secondaries -= x
      }}
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }
    case Insert(key, value, id) => {
      kv +=  key -> value
      replicate(key, Some(value), id)
    }
    case Remove(key, id) => {
      kv -= key
      replicate(key, None, id)
    }
    case Replicator.Replicated(key, id) => {
      BothAck.replicationAcked(key, id, sender)
    }
    case Persistence.Persisted(key, id) => {
      BothAck.persistenceAcked(key, id)
    }
    case ScheduleCheck(key, id) => {
      BothAck.checkFail(key, id)
    }
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = LoggingReceive {
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }
    case Insert(key, value, id) => {
      sender ! OperationFailed(id)
    }
    case Remove(key, id) => {
      sender ! OperationFailed(id)
    }
    case Replicator.Snapshot(key, valueOption, seq) => {
      if (seq == expectedSeq) {
        // update kv
        valueOption match {
          case Some(value) => kv += (key -> value)
          case None => kv -= key
        }
        // persist
        persist(key, valueOption, seq)
        // update seq
        expectedSeq = Math.max(expectedSeq, seq+1)
        // sender ! Replicator.SnapshotAck(key, seq)
      } else if (seq < expectedSeq) {
        sender ! Replicator.SnapshotAck(key, seq)
      }
    }
    case Persisted(key, seq) => {
      persistenceIdToClient(seq)._1 ! SnapshotAck(key, seq)
      persistenceIdToClient -= seq
    }
  }

  case class BothAck(
                      key: String,
                      id: Long,
                      pendingReplicators: Set[ActorRef] = Set.empty[ActorRef],
                      client: ActorRef,
                      persistenceAcked: Boolean = false ){
    val acked = persistenceAcked && pendingReplicators.isEmpty
  }

  object BothAck {
    var pendingRequests = Map.empty[(String, Long), BothAck] // (key, id) -> bothAck

    def persistenceAcked(key: String, id: Long) = {
      pendingRequests get (key, id) foreach { ack => {
        checkDone(ack.copy(persistenceAcked = true))
      }}
    }

    def replicationAcked(key: String, id: Long, replicator: ActorRef) = {
      pendingRequests get (key, id) foreach { ack => {
        checkDone(ack.copy( pendingReplicators = ack.pendingReplicators - replicator))
      }}
    }

    private def checkDone(ack :BothAck): Unit = {
      if (ack.acked) {
        ack.client ! OperationAck(ack.id)
        pendingRequests -= Tuple2(ack.key, ack.id)
      } else {
        pendingRequests += Tuple2(ack.key, ack.id) -> ack
      }
    }

    def checkFail(key: String, id: Long): Unit = {
      pendingRequests get (key, id) foreach { ack => {
        ack.client ! OperationFailed(id)
        pendingRequests -= Tuple2(key, id)
      }}
    }

    def findRequest(key: String, id: Long, client: ActorRef): BothAck = {
      pendingRequests.getOrElse((key, id), BothAck(key = key, id = id, client = client))
    }

    def registerPersistence(key: String, id: Long, client: ActorRef): Unit = {
      pendingRequests += Tuple2(key, id) -> findRequest(key, id, client).copy(persistenceAcked = true)
    }

    def registerReplication(key: String, id: Long, replicator: ActorRef, client: ActorRef): Unit = {
      val found = findRequest(key, id, client)
      pendingRequests += Tuple2(key, id) -> found.copy( pendingReplicators = found.pendingReplicators + replicator)
    }

    def removeReplicator(toRemove: ActorRef): Unit = {
      pendingRequests.values.foreach { ack => {
        checkDone(ack.copy(pendingReplicators = ack.pendingReplicators - toRemove))
      }}
    }
  }
}

