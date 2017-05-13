object week5Sheet {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(63); 
  println("Welcome to the Scala worksheet");$skip(144); 

  def squareList_Pattern(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => (y * y) :: squareList_Pattern(ys)
  };System.out.println("""squareList_Pattern: (xs: List[Int])List[Int]""");$skip(73); 
  def squareList_Map(xs: List[Int]): List[Int] =
    xs map (x => x * x);System.out.println("""squareList_Map: (xs: List[Int])List[Int]""");$skip(125); 


  def mapFunL[T, U](xs: List[T], f: T => U): List[U] =
  	(xs foldLeft List[U]())((lst, item) => f(item) :: lst) reverse;System.out.println("""mapFunL: [T, U](xs: List[T], f: T => U)List[U]""");$skip(117); 
  
  def mapFunR[T, U](xs: List[T], f: T => U): List[U] =
  	(xs foldRight List[U]())((item, lst) => f(item) :: lst);System.out.println("""mapFunR: [T, U](xs: List[T], f: T => U)List[U]""");$skip(81); 
                                                  
  val l = List(1, 2, 3, 4, 5);System.out.println("""l  : List[Int] = """ + $show(l ));$skip(32); val res$0 = 
  mapFunL(l, (x: Int) => x * x);System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(32); val res$1 = 
  mapFunR(l, (x: Int) => x * x);System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(180); 
  
    //f(xs.head) :: (xs foldRight Nil)(f)
    
    //((x, xss) => f(x) :: mapFun(xss, f))


  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((h, acc) => acc + 1);System.out.println("""lengthFun: [T](xs: List[T])Int""")}
}