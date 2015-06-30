package simulations

import common._
import scala.Predef._
import scala.math.pow

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig || a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire): Unit = {
    val a3, a4, a5: Wire = new Wire
    inverter(a1, a3)
    inverter(a2, a4)
    andGate(a3, a4, a5)
    inverter(a5, output)
  }

  def deplexer(in: Wire, c: Wire, outHigh: Wire, outLow: Wire): Unit = {
    val cLow = new Wire
    inverter(c, cLow)
    andGate(in, c, outHigh)
    andGate(in, cLow, outLow)
  }

  // The list of control wires (c) and the the list of output wires (out) are assumed to be sorted by decreasing index: e.g. the head of the c list contains the control wire of index n-1
  def demux(in: Wire, c: List[Wire], out: List[Wire]): Unit = {
    // assert |out| = 2 ^ |c|
    require(out.length == pow(2, c.length))
    def demuxIntern(in: Wire, c: List[Wire], out: List[Wire]): Unit = c match {
      case cin::cx => {
        val inHigh, inLow = new Wire
        deplexer(in, cin, inHigh, inLow)
        // care for the list order
        val outHigh = out take (out.length/2)
        val outLow = out drop (out.length/2)
        demuxIntern(inHigh, cx, outHigh)
        demuxIntern(inLow, cx, outLow)
      }
      case List() => {
        require(out.length == 1)
        def setAction(): Unit = {
          val sig = in.getSignal
          afterDelay(0){ out(0).setSignal(sig) }
        }
        in addAction setAction
      }
    }
    demuxIntern(in, c, out)
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
