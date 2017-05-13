package funsets
import FunSets._

object testSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  //type Set = Int => Boolean

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val odds = (i: Int) => i % 2 == 1               //> odds  : Int => Boolean = <function1>

  contains(odds, 3)                               //> res0: Boolean = true
  contains(odds, 17)                              //> res1: Boolean = true

  val evens = map(odds, i => i + 1)               //> evens  : Int => Boolean = <function1>
  contains(evens, 42)                             //> res2: Boolean = true

  var doubles = map(odds, i => i * 2)             //> doubles  : Int => Boolean = <function1>
  //printSet(doubles)
  doubles = map(evens, i => i * 2)
  printSet(doubles)                               //> {4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,1
                                                  //| 04,108,112,116,120,124,128,132,136,140,144,148,152,156,160,164,168,172,176,1
                                                  //| 80,184,188,192,196,200,204,208,212,216,220,224,228,232,236,240,244,248,252,2
                                                  //| 56,260,264,268,272,276,280,284,288,292,296,300,304,308,312,316,320,324,328,3
                                                  //| 32,336,340,344,348,352,356,360,364,368,372,376,380,384,388,392,396,400,404,4
                                                  //| 08,412,416,420,424,428,432,436,440,444,448,452,456,460,464,468,472,476,480,4
                                                  //| 84,488,492,496,500,504,508,512,516,520,524,528,532,536,540,544,548,552,556,5
                                                  //| 60,564,568,572,576,580,584,588,592,596,600,604,608,612,616,620,624,628,632,6
                                                  //| 36,640,644,648,652,656,660,664,668,672,676,680,684,688,692,696,700,704,708,7
                                                  //| 12,716,720,724,728,732,736,740,744,748,752,756,760,764,768,772,776,780,784,7
                                                  //| Output exceeds cutoff limit. 
}