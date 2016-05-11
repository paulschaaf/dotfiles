package funsets
import FunSets._

object testSets {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(232); 
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  //type Set = Int => Boolean

  println("Welcome to the Scala worksheet");$skip(38); 

  val odds = (i: Int) => i % 2 == 1;System.out.println("""odds  : Int => Boolean = """ + $show(odds ));$skip(22); val res$0 = 

  contains(odds, 3);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(21); val res$1 = 
  contains(odds, 17);System.out.println("""res1: Boolean = """ + $show(res$1));$skip(38); 

  val evens = map(odds, i => i + 1);System.out.println("""evens  : Int => Boolean = """ + $show(evens ));$skip(22); val res$2 = 
  contains(evens, 42);System.out.println("""res2: Boolean = """ + $show(res$2));$skip(40); 

  var doubles = map(odds, i => i * 2);System.out.println("""doubles  : Int => Boolean = """ + $show(doubles ));$skip(57); 
  //printSet(doubles)
  doubles = map(evens, i => i * 2);$skip(20); 
  printSet(doubles)}
}