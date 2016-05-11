object IntSet {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(59); 
  println("Welcome to the Scala worksheet");$skip(62); 

  val a = new NonEmpty(4, new NonEmpty(2), new NonEmpty(6));System.out.println("""a  : NonEmpty = """ + $show(a ));$skip(40); 
  val b = new NonEmpty(3) incl 1 incl 5;System.out.println("""b  : IntSet = """ + $show(b ));$skip(20); 
  val c = a union b;System.out.println("""c  : IntSet = """ + $show(c ));$skip(4); val res$0 = 
  c;System.out.println("""res0: IntSet = """ + $show(res$0));$skip(13); val res$1 = 
	c.toString2;System.out.println("""res1: java.lang.String = """ + $show(res$1))}
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