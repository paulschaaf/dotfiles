object IntSet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val a = new NonEmpty(4, new NonEmpty(2), new NonEmpty(6))
                                                  //> a  : NonEmpty = ./2\./4\./6\.
  val b = new NonEmpty(3) incl 1 incl 5           //> b  : IntSet = ./1\./3\./5\.
  val c = a union b                               //> c  : IntSet = ./1\./2\./3\./4\./5\./6\.
  c                                               //> res0: IntSet = ./1\./2\./3\./4\./5\./6\.
	c.toString2                               //> res1: java.lang.String = ((./1\(./2\.))/3\((./4\.)/5\(./6\.)))
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
  def toString2 = toString
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  def union(other: IntSet): IntSet = other
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	def this(elem: Int) = this(elem, new Empty, new Empty)
	
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  override def toString2 = "(" + (left.toString2) + "/" + elem + "\\" + (right.toString2) + ")"
  override def toString = left + "/" + elem + "\\" + right
//  override def toString = "(" + left + ")" + elem + "(" + right + ")"

  def union(other: IntSet): IntSet = left union right union other incl elem

}