import streams._
import week7._
import Bloxorz._

object Worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  var level =
      """ST
        |oo
        |oo""".stripMargin                        //> level  : String = ST
                                                  //| oo
                                                  //| oo
	val levelVector = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
                                                  //> levelVector  : scala.collection.immutable.Vector[scala.collection.immutable.
                                                  //| Vector[Char]] = Vector(Vector(S, T), Vector(o, o), Vector(o, o))
	levelVector(1)                            //> res0: scala.collection.immutable.Vector[Char] = Vector(o, o)
	levelVector(1)(1)                         //> res1: Char = o
	Vector().length                           //> res2: Int = 0
	(1, 4) == (1, 4)                          //> res3: Boolean = true

  case class Pos(x: Int, y: Int) {
    /** The position obtained by changing the `x` coordinate by `d` */
    def dx(d: Int) = copy(x = x + d, y)

    /** The position obtained by changing the `y` coordinate by `d` */
    def dy(d: Int) = copy(x, y = y + d)
  }

	Pos(1, 4) == Pos(1, 3)                    //> res4: Boolean = false

	val problem = new Pouring(Vector(4, 9))   //> starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set of size: 1
                                                  //| starting allPathsStartingFrom with paths set
                                                  //| Output exceeds cutoff limit.
	problem.solutions(6)
	
	//var solvr = new Solver()

	//val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
    
}