import math.abs

object worksheet {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(79); 
  println("Welcome to the Scala worksheet");$skip(204); 

  //@tailrec
  def fact(n: Int): Int = {
    def factImpl(total: Int, current: Int): Int = {
      if (current == 0) total
      else factImpl(total * current, current - 1)
    }
    factImpl(1, n)
  };System.out.println("""fact: (n: Int)Int""");$skip(199); 

  //fact(4)
  //fact(5)

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  };System.out.println("""sum: (f: Int => Int)(a: Int, b: Int)Int""");$skip(114); 

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  };System.out.println("""product: (f: Int => Int)(a: Int, b: Int)Int""");$skip(30); val res$0 = 

  product(x => x * x)(3, 4);System.out.println("""res0: Int = """ + $show(res$0));$skip(48); 

	def factorial(n: Int) = product(x => x)(1, n);System.out.println("""factorial: (n: Int)Int""");$skip(16); val res$1 = 
	
	factorial(4);System.out.println("""res1: Int = """ + $show(res$1));$skip(14); val res$2 = 
	factorial(5);System.out.println("""res2: Int = """ + $show(res$2));$skip(354); 
	
	def reduce(initial: Int, reducer: (Int, Int) => Int)(start: Int, end: Int): Int = {
/*		def reduceImpl(total: Int, current: Int): Int = {
			if (current > end) total
			else reduceImpl(reducer(total, current), current+1)
		}
		reduceImpl(initial, start)*/
		
		if (start > end) initial
		else reduce(reducer(initial, start), reducer)(start+1, end)
	};System.out.println("""reduce: (initial: Int, reducer: (Int, Int) => Int)(start: Int, end: Int)Int""");$skip(108); 
	
	def product2(start: Int, end: Int) = {
		reduce(1, (total: Int, next: Int) => total*next)(start, end)
	};System.out.println("""product2: (start: Int, end: Int)Int""");$skip(67); 

	val product3 = reduce(1, (total: Int, next: Int) => total*next)_;System.out.println("""product3  : (Int, Int) => Int = """ + $show(product3 ));$skip(18); val res$3 = 
	
	product2(2, 5);System.out.println("""res3: Int = """ + $show(res$3));$skip(16); val res$4 = 
	product3(2, 5);System.out.println("""res4: Int = """ + $show(res$4));$skip(96); 

	def sum2(start: Int, end: Int) = reduce(0, (total: Int, next: Int) => total+next)(start, end);System.out.println("""sum2: (start: Int, end: Int)Int""");$skip(64); 
	
	val sum3 = reduce(0, (total: Int, next: Int) => total+next)_;System.out.println("""sum3  : (Int, Int) => Int = """ + $show(sum3 ));$skip(14); val res$5 = 
	
	sum2(1, 3);System.out.println("""res5: Int = """ + $show(res$5));$skip(12); val res$6 = 
	sum3(1, 3);System.out.println("""res6: Int = """ + $show(res$6));$skip(170); 
	
	def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
		if (a > b) zero
		else combine(f(a), mapReduce(f, combine, zero)(a+1, b));System.out.println("""mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int)Int""");$skip(141); 
                                                  
  def product4(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x*y, 1)(a, b);System.out.println("""product4: (f: Int => Int)(a: Int, b: Int)Int""");$skip(76); val res$7 = 
                                                  
  product4(x => x)(2, 5);System.out.println("""res7: Int = """ + $show(res$7));$skip(30); 
  
  
	val tolerance = 0.0001;System.out.println("""tolerance  : Double = """ + $show(tolerance ));$skip(76); 
	def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) / x < tolerance;System.out.println("""isCloseEnough: (x: Double, y: Double)Boolean""");$skip(345); 
                                                  
	// repeat application of f() until it converges (within a tolerance)
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  	def iterate(guess: Double): Double = {
  		val next = f(guess)
  		if (isCloseEnough(guess, next)) next
  		else iterate(next)
  	}
  	iterate(firstGuess)
  };System.out.println("""fixedPoint: (f: Double => Double)(firstGuess: Double)Double""");$skip(33); val res$8 = 
  
  fixedPoint(x => 1 + x/2)(1);System.out.println("""res8: Double = """ + $show(res$8));$skip(68); 
  
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2;System.out.println("""averageDamp: (f: Double => Double)(x: Double)Double""");$skip(131); 
                                                  
  def sqrt2(x: Double): Double = {
		fixedPoint(averageDamp(y => x / y))(1)
  };System.out.println("""sqrt2: (x: Double)Double""");$skip(11); val res$9 = 
  sqrt2(2);System.out.println("""res9: Double = """ + $show(res$9))}
}