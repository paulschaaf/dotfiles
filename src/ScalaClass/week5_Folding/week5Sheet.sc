object week5Sheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def squareList_Pattern(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => (y * y) :: squareList_Pattern(ys)
  }                                               //> squareList_Pattern: (xs: List[Int])List[Int]
  def squareList_Map(xs: List[Int]): List[Int] =
    xs map (x => x * x)                           //> squareList_Map: (xs: List[Int])List[Int]


  def mapFunL[T, U](xs: List[T], f: T => U): List[U] =
  	(xs foldLeft List[U]())((lst, item) => f(item) :: lst) reverse
                                                  //> mapFunL: [T, U](xs: List[T], f: T => U)List[U]
  
  def mapFunR[T, U](xs: List[T], f: T => U): List[U] =
  	(xs foldRight List[U]())((item, lst) => f(item) :: lst)
                                                  //> mapFunR: [T, U](xs: List[T], f: T => U)List[U]
                                                  
  val l = List(1, 2, 3, 4, 5)                     //> l  : List[Int] = List(1, 2, 3, 4, 5)
  mapFunL(l, (x: Int) => x * x)                   //> res0: List[Int] = List(1, 4, 9, 16, 25)
  mapFunR(l, (x: Int) => x * x)                   //> res1: List[Int] = List(1, 4, 9, 16, 25)
  
    //f(xs.head) :: (xs foldRight Nil)(f)
    
    //((x, xss) => f(x) :: mapFun(xss, f))


  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((h, acc) => acc + 1)         //> lengthFun: [T](xs: List[T])Int
}