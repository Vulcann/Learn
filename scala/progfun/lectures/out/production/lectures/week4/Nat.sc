
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat{
  def isZero: Boolean = true
  def predecessor: Nat = throw new Error("0.predecessor")
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat{
  def isZero: Boolean = false
  def predecessor: Nat = n
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = {
//    if (that == Zero) this
//    else {
//      successor + n
//    }
    that.successor + n // tail recursive
    // new Succ(that + n)
  }
  def - (that: Nat): Nat = {
//    if (that == Zero) this
//    else {
//      predecessor - n
//    }
      if (that.isZero) this
      else n - that.predecessor
  }
}
