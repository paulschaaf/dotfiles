import streams._
import week7._
import Bloxorz._

object Worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(112); 
  println("Welcome to the Scala worksheet");$skip(65); 
  var level =
      """ST
        |oo
        |oo""".stripMargin;System.out.println("""level  : String = """ + $show(level ));$skip(80); 
	val levelVector = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'));System.out.println("""levelVector  : scala.collection.immutable.Vector[scala.collection.immutable.Vector[Char]] = """ + $show(levelVector ));$skip(16); val res$0 = 
	levelVector(1);System.out.println("""res0: scala.collection.immutable.Vector[Char] = """ + $show(res$0));$skip(19); val res$1 = 
	levelVector(1)(1);System.out.println("""res1: Char = """ + $show(res$1));$skip(17); val res$2 = 
	Vector().length;System.out.println("""res2: Int = """ + $show(res$2));$skip(18); val res$3 = 
	(1, 4) == (1, 4)

  case class Pos(x: Int, y: Int) {
    /** The position obtained by changing the `x` coordinate by `d` */
    def dx(d: Int) = copy(x = x + d, y)

    /** The position obtained by changing the `y` coordinate by `d` */
    def dy(d: Int) = copy(x, y = y + d)
  };System.out.println("""res3: Boolean = """ + $show(res$3));$skip(288); val res$4 = 

	Pos(1, 4) == Pos(1, 3);System.out.println("""res4: Boolean = """ + $show(res$4));$skip(42); 

	val problem = new Pouring(Vector(4, 9));System.out.println("""problem  : week7.Pouring = """ + $show(problem ));$skip(22); val res$5 = 
	problem.solutions(6);System.out.println("""res5: Stream[Worksheet.problem.Path] = """ + $show(res$5))}
	
	//var solvr = new Solver()

	//val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
    
}