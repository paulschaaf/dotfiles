object nqueens {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(60); 
  println("Welcome to the Scala worksheet");$skip(303); 

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
  };System.out.println("""queens: (n: Int)Set[List[Int]]""");$skip(292); 
  def isSafe(col: Int, queens: List[Int]): Boolean = {
  	val row = queens.length
  	val queensWithRow = (row - 1 to 0 by -1) zip queens
  	queensWithRow forall {
  		case (r, c) => col != c && math.abs(col - c) != row - r // check the column and diagonal
  	}
  	//!queens.contains(col)
  };System.out.println("""isSafe: (col: Int, queens: List[Int])Boolean""");$skip(13); val res$0 = 

	queens(4)

	import polynomials.Poly;System.out.println("""res0: Set[List[Int]] = """ + $show(res$0));$skip(70); 
	
	val p = new Poly(1->2.0, 3->4.0, 5->6.2);System.out.println("""p  : polynomials.Poly = """ + $show(p ));$skip(11); 
	val q = p;System.out.println("""q  : polynomials.Poly = """ + $show(q ));$skip(12); val res$1 = 
	q.terms(1);System.out.println("""res1: Double = """ + $show(res$1));$skip(8); val res$2 = 
 	q + p;System.out.println("""res2: polynomials.Poly = """ + $show(res$2));$skip(9); val res$3 = 
 	q ++ p;System.out.println("""res3: polynomials.Poly = """ + $show(res$3));$skip(29); val res$4 = 
  List("abcd", "e").mkString;System.out.println("""res4: String = """ + $show(res$4));$skip(48); val res$5 = 
  
  for {
  	times <- 1 to 7
  } yield times*2;System.out.println("""res5: scala.collection.immutable.IndexedSeq[Int] = """ + $show(res$5))}
                                                  
}