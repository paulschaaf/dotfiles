package NaturalNumbers

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
  override def toString = "/*" + toInt + "*/"
  def toInt: Int
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def +(that: Nat) = that
  def -(that: Nat) = if (that.isZero) this else throw new Error("negative number")
  def toInt = 0
}

class Succ(val predecessor: Nat) extends Nat {
  def isZero = false
  def +(that: Nat) = {
    print(" -> " + toInt + "+" + that.toInt)
    // move one from this to that recursively
    predecessor + that.successor   // M.O. used new Succ(predecessor + that)
  }
  def -(that: Nat) = {
    print(" -> " + toInt + "-" + that.toInt)
    if (that.isZero) this
    // subtract one from each until the second is zero
    else predecessor - that.predecessor
  }

  def toInt = 1 + predecessor.toInt
}
