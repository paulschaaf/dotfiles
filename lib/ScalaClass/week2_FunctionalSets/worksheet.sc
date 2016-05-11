import math.abs

object worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  //@tailrec
  def fact(n: Int): Int = {
    def factImpl(total: Int, current: Int): Int = {
      if (current == 0) total
      else factImpl(total * current, current - 1)
    }
    factImpl(1, n)
  }                                               //> fact: (n: Int)Int

  //fact(4)
  //fact(5)

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int)(a: Int, b: Int)Int

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int

  product(x => x * x)(3, 4)                       //> res0: Int = 144

	def factorial(n: Int) = product(x => x)(1, n)
                                                  //> factorial: (n: Int)Int
	
	factorial(4)                              //> res1: Int = 24
	factorial(5)                              //> res2: Int = 120
	
	def reduce(initial: Int, reducer: (Int, Int) => Int)(start: Int, end: Int): Int = {
/*		def reduceImpl(total: Int, current: Int): Int = {
			if (current > end) total
			else reduceImpl(reducer(total, current), current+1)
		}
		reduceImpl(initial, start)*/
		
		if (start > end) initial
		else reduce(reducer(initial, start), reducer)(start+1, end)
	}                                         //> reduce: (initial: Int, reducer: (Int, Int) => Int)(start: Int, end: Int)Int
                                                  //| 
	
	def product2(start: Int, end: Int) = {
		reduce(1, (total: Int, next: Int) => total*next)(start, end)
	}                                         //> product2: (start: Int, end: Int)Int

	val product3 = reduce(1, (total: Int, next: Int) => total*next)_
                                                  //> product3  : (Int, Int) => Int = <function2>
	
	product2(2, 5)                            //> res3: Int = 120
	product3(2, 5)                            //> res4: Int = 120

	def sum2(start: Int, end: Int) = reduce(0, (total: Int, next: Int) => total+next)(start, end)
                                                  //> sum2: (start: Int, end: Int)Int
	
	val sum3 = reduce(0, (total: Int, next: Int) => total+next)_
                                                  //> sum3  : (Int, Int) => Int = <function2>
	
	sum2(1, 3)                                //> res5: Int = 6
	sum3(1, 3)                                //> res6: Int = 6
	
	def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
		if (a > b) zero
		else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b
                                                  //| : Int)Int
                                                  
  def product4(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x*y, 1)(a, b)
                                                  //> product4: (f: Int => Int)(a: Int, b: Int)Int
                                                  
  product4(x => x)(2, 5)                          //> res7: Int = 120
  
  
	val tolerance = 0.0001                    //> tolerance  : Double = 1.0E-4
	def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) / x < tolerance
                                                  //> isCloseEnough: (x: Double, y: Double)Boolean
                                                  
	// repeat application of f() until it converges (within a tolerance)
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  	def iterate(guess: Double): Double = {
  		val next = f(guess)
  		if (isCloseEnough(guess, next)) next
  		else iterate(next)
  	}
  	iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  
  fixedPoint(x => 1 + x/2)(1)                     //> res8: Double = 1.999755859375
  
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double
                                                  //| Output exceeds cutoff limit. 
                                                  
  def sqrt2(x: Double): Double = {
		fixedPoint(averageDamp(y => x / y))(1)
  }
  sqrt2(2)
}