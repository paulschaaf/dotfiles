import week4.{List, Cons, Nil}
import Expressions._

object worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(115); 
  println("Welcome to the Scala worksheet");$skip(810); val res$0 = 
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
	Var("x").show;System.out.println("""res0: String = """ + $show(res$0));$skip(16); val res$1 = 
	Number(3).show;System.out.println("""res1: String = """ + $show(res$1));$skip(32); val res$2 = 
	Prod(Number(2), Var("x")).show;System.out.println("""res2: String = """ + $show(res$2));$skip(47); val res$3 = 
	Sum(Prod(Number(2), Var("x")), Var("y")).show;System.out.println("""res3: String = """ + $show(res$3));$skip(47); val res$4 = 
	Prod(Sum(Number(2), Var("x")), Var("y")).show;System.out.println("""res4: String = """ + $show(res$4));$skip(64); val res$5 = 
	Sum(Prod(Number(2), Var("x")), Prod(Number(2), Var("x"))).show;System.out.println("""res5: String = """ + $show(res$5));$skip(63); val res$6 = 
	Prod(Sum(Number(2), Var("x")), Sum(Number(2), Var("x"))).show;System.out.println("""res6: String = """ + $show(res$6));$skip(47); val res$7 = 
	Prod(Var("y"), Sum(Number(2), Var("x"))).show;System.out.println("""res7: String = """ + $show(res$7))}
}