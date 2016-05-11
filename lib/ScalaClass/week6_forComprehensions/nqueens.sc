object nqueens {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def queens(n: Int) = { // Lecture 6.3
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }
    placeQueens(n)
  }                                               //> queens: (n: Int)Set[List[Int]]
  def isSafe(col: Int, queens: List[Int]): Boolean = {
  	val row = queens.length
  	val queensWithRow = (row - 1 to 0 by -1) zip queens
  	queensWithRow forall {
  		case (r, c) => col != c && math.abs(col - c) != row - r // check the column and diagonal
  	}
  	//!queens.contains(col)
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean

	queens(4)                                 //> res0: Set[List[Int]] = Set(List(1, 3, 0, 2), List(2, 0, 3, 1))

	import polynomials.Poly
	
	val p = new Poly(1->2.0, 3->4.0, 5->6.2)  //> p  : polynomials.Poly = 6.2x^5 + 4.0x^3 + 2.0x^1
	val q = p                                 //> q  : polynomials.Poly = 6.2x^5 + 4.0x^3 + 2.0x^1
	q.terms(1)                                //> res1: Double = 2.0
 	q + p                                     //> res2: polynomials.Poly = 12.4x^5 + 8.0x^3 + 4.0x^1
 	q ++ p                                    //> res3: polynomials.Poly = 12.4x^5 + 8.0x^3 + 4.0x^1
  List("abcd", "e").mkString                      //> res4: String = abcde
  
  for {
  	times <- 1 to 7
  } yield times*2                                 //> res5: scala.collection.immutable.IndexedSeq[Int] = Vector(2, 4, 6, 8, 10, 12
                                                  //| , 14)
                                                  
}