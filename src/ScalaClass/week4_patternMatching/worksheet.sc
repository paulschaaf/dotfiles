import week4.{List, Cons, Nil}
import Expressions._

object worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
/*
  //val lst = List(1, 2, 3, 4, 5, 6, 7)
  val lst = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5, new Cons(6, new Cons(7, new Nil)))))))
  lst.head
  lst.tail
  lst.nth(4)
  
  def nth[T](n: Int, l: List[T]): T = {
		if (l.isEmpty || n < 0) throw new IndexOutOfBoundsException
		if (n == 0) l.head
		else nth(n - 1, l.tail)
  }
  nth(4, lst)
//  nth(7, lst)
                                                  
  import NaturalNumbers._
  
	val one   = Zero.successor
	val two   = one.successor
  val three = two.successor
  three.predecessor
	three
  three.successor
  three.predecessor.predecessor
  //three - three.predecessor
	three + Zero
	Zero + three
	three + one
	three + two
   three + three
	Zero + three
  three - one
  three - two

	List(1, 2).tail.tail
	//List()
 */
	Var("x").show                             //> res0: String = x
	Number(3).show                            //> res1: String = 3
	Prod(Number(2), Var("x")).show            //> res2: String = 2 * x
	Sum(Prod(Number(2), Var("x")), Var("y")).show
                                                  //> res3: String = 2 * x + y
	Prod(Sum(Number(2), Var("x")), Var("y")).show
                                                  //> res4: String = (2 + x) * y
	Sum(Prod(Number(2), Var("x")), Prod(Number(2), Var("x"))).show
                                                  //> res5: String = 2 * x + 2 * x
	Prod(Sum(Number(2), Var("x")), Sum(Number(2), Var("x"))).show
                                                  //> res6: String = (2 + x) * (2 + x)
	Prod(Var("y"), Sum(Number(2), Var("x"))).show
                                                  //> res7: String = y * (2 + x)
}