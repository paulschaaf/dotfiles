import scala.collection.BitSet
import java.math.BigInteger

object Euler {

import scala.annotation.tailrec

// Problem 1
// If we list all the natural numbers below 10 that are multiples of 3 or 5, we
// get 3, 5, 6 and 9. The sum of these multiples is 23.
// Find the sum of all the multiples of 3 or 5 below 1000.
val cap = 1000                                    //> cap  : Int = 1000
def problem1(iter: Int): Set[Int] =
  	(iter*3, iter*5) match {
  		// If *3 is out of range there are no more solutions.
			case (x3, _) if x3 >= cap => Set()  // The wildcard "_" means the *5 term is irrelevant.
			
			// *3 is ok but *5 is out of range, so find the remaining terms then add the current multiple of 3
			case (x3, x5) if x5 >= cap => problem1(iter+1) + x3
			
			// find the remaining terms then add the current multiples of 3 and 5
			case (x3, x5) => problem1(iter+1) + x3 + x5
		}                                 //> problem1: (iter: Int)Set[Int]
//problem1(1).sum


// Problem 2
// By considering the terms in the Fibonacci sequence whose values do not exceed four million,
// find the sum of the even-valued terms.

// This is an infinite stream that "contains" all values in the sequence
val fibonacciSequence = {
	def fib(x: Int, x1: Int): Stream[Int] = x #:: fib(x1, x+x1)
	fib(1, 2)
}                                                 //> fibonacciSequence  : Stream[Int] = Stream(1, ?)

/*fibonacciSequence.         // out of the entire sequence ...
	filter(_ % 2 == 0).      // considering only the even numbers ...
	takeWhile(_ < 4000000).  // that are below 4 million ...
	sum                      // calculate their sum
*/


// Problem 3
// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143 ?
5*7*13*29                                         //> res0: Int = 13195

// I repeatedly reduce the number by its smallest factor. If that factor is the number itself,
// it must be prime. For instance, we reduce 60 by dividing by 2, 2, and 3: 60 => 30 => 15 => 5.
// 5 has no smaller factors, so it's the largest prime factor.
def largestPrimeFactorOf(num: BigInt): BigInt = {

	// Scala compiles this tail-recursive method into an iterative loop
	def lgstPrime(target: BigInt, startingFrom: BigInt): BigInt = {
	  // if we've removed all possible factors what remains must be prime
	  if (startingFrom == target) target
	  
	  // reduce by this factor and try with the new number
	  else if (target % startingFrom == 0) lgstPrime(target/startingFrom, startingFrom)
	  
	  // startingFrom isn't a factor, try its successor
	  else lgstPrime(target, startingFrom + 1)
  }
  
  if (num < 2) num  // without this there's an infinite loop
  else lgstPrime(num, 2)
}                                                 //> largestPrimeFactorOf: (num: BigInt)BigInt

/*
largestPrimeFactorOf(BigInt("600851475143"))
largestPrimeFactorOf(1)
largestPrimeFactorOf(0)
largestPrimeFactorOf(13195)
*/

// Problem 4
// A palindromic number reads the same both ways. The largest palindrome made from the product of two
// 2-digit numbers is 9009 = 91 * 99.
// Find the largest palindrome made from the product of two 3-digit numbers.

// in other words, find the first palindrome between the largest possible product of 3-digit numbers
// (999*999 == 998,001) and the smallest possible (100**2 == 10,000) that has two 3-digit numbers as factors.
def threeDigitFactorsOf(dividend: Int): Stream[Int] = {
	(100 to 999)                                                  // from the smallest 3-digit number to the largest
		.toStream                                                   // don't precompute the whole list; do it lazily
		.filter(divisor => dividend % divisor == 0)                 // discard any non-factors
		.filter(divisor => (dividend/divisor).toString.length == 3) // keep only the 3-digit results
}                                                 //> threeDigitFactorsOf: (dividend: Int)Stream[Int]

threeDigitFactorsOf(222*222)                      //> res1: Stream[Int] = Stream(111, ?)
threeDigitFactorsOf((222*333) - 1)                //> res2: Stream[Int] = Stream()

val palindromes = {
	def palStream(start: Int): Stream[Int] = (start + start.toString.reverse).toInt #:: palStream(start - 1)
	palStream(999) // the largest possible product of 3-digit numbers is 999*999 == 998,001
}                                                 //> palindromes  : Stream[Int] = Stream(999999, ?)

val threeDigFactors = palindromes
	.takeWhile(_ > 0)  // keep going until we reach the first 3 digit number
	.filter(threeDigitFactorsOf(_).nonEmpty)  // keep only those with three digit factors
  .head                                           //> threeDigFactors  : Int = 906609

// Problem 5
// What is the smallest number divisible by each of the numbers 1 to 20?
def intDiv(num: Int, factor :Int) = if (num % factor == 0) num/factor else num
                                                  //> intDiv: (num: Int, factor: Int)Int

val factors = {
	def f(factorList: List[Int], num: Int): Stream[Int] = {
		if (num <= 20) {
			val reducedTerm = factorList.foldLeft(num)((factor, e) => intDiv(factor, e)) // reduce by each factor already encountered
			reducedTerm #:: f(reducedTerm :: factorList, num+1)
		}
		else Stream.empty
	}
	f(Nil, 1)
}                                                 //> factors  : Stream[Int] = Stream(1, ?)
factors.product                                   //> res3: Int = 232792560

val a = (1, 2)                                    //> a  : (Int, Int) = (1,2)

// Problem 6
// The sum of the squares of the first ten natural numbers is 1^2 + 2^2 + ... + 10^2 = 385
// The square of the sum of the first ten natural numbers is (1 + 2 + ... + 10)^2 = 552 = 3025
// Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.
//
//Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

/*
val sums = (1 to 100).foldLeft((0, 0))((totals, nextTerm) => (totals._1 + nextTerm*nextTerm, totals._2 + nextTerm))
val sumOfSquares = sums._1
val squareOfSums = sums._2*sums._2
squareOfSums - sumOfSquares
*/


// Problem 7
// Find the 10,001st prime.
/*
	def isPrime(num: Int, primes: List[Int]): Boolean = {
		val sqrt = Math.sqrt(num)
		def hasFactorIn(factors: List[Int]): Boolean = factors match {
			case Nil => false
			case h :: _ if h > sqrt => false
			case h :: _ if num % h == 0 => true
			case _ :: t => hasFactorIn(t)
		}
		!hasFactorIn(primes)
	}
	def sieve2(num: Int, primes: List[Int]): Stream[Int] = {
		if (isPrime(num, primes)) sieve2(num + 2, num :: primes)
		else sieve2(num + 2, primes)
	}
*/
val primes2 = {
	// Sieving integral numbers
  //@tailrec
  def sieve(s: Stream[Int]): Stream[Int] = {
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  }

  // All primes as a lazy sequence
  sieve(Stream.from(2))
}                                                 //> primes2  : Stream[Int] = Stream(2, ?)

def primesUntil(stopWhen: ((Double, Sequence[Double]) => Boolean)): Sequence[Double] = {
	@tailrec
	def sieveCheckFactors(num: Double, primesSoFar: Sequence[Double]): Sequence[Double] = {
		if (stopWhen(num, primesSoFar)) primesSoFar
		else {
  	  val sqrt = scala.Math.sqrt(num.toDouble)
  	  if (primesSoFar.takeWhile(_ <= sqrt).exists(num % _ == 0)) sieveCheckFactors(num + 2, primesSoFar)
  		else sieveCheckFactors(num + 2, primesSoFar :+ num)
  	}
	}
	//val seq: Sequence
	sieveCheckFactors(3, Sequence(2))
}                                                 //> primesUntil: (stopWhen: (Double, Sequence[Double]) => Boolean)Sequence[Doub
                                                  //| le]
//val primes = List(2)

//primes.take(20).toList
//primes2.take(20).toList
val pu = primesUntil((num, prms) => prms.length == 10)//001).last
                                                  //> pu  : Sequence[Double] = List(2.0, 3.0, 5.0, 7.0, 11.0, 13.0, 17.0, 19.0, 2
                                                  //| 3.0, 29.0)
println("---")                                    //> ---
println(pu.sum)                                   //> 129.0
//primes(10000)
//primes2(10000)

object Prime3 {
	def primesUnder(n: Int): List[Int] = {
	  require(n >= 2)
	
		@tailrec
	  def rec(i: Int, primes: List[Int]): List[Int] = {
	    if (i >= n) primes
	    else if (prime(i, primes)) rec(i + 1, i :: primes)
	    else rec(i + 1, primes)
	  }
	
	  rec(2, List()).reverse
	}
	
	def prime(num: Int, factors: List[Int]): Boolean = factors.forall(num % _ != 0)
}


// Problem 8
// Find the greatest product of five consecutive digits in the 1000-digit number.
//
val thousandDigits = "73167176531330624919225119674426574742355349194934" +
	"96983520312774506326239578318016984801869478851843" +
	"85861560789112949495459501737958331952853208805511" +
	"12540698747158523863050715693290963295227443043557" +
	"66896648950445244523161731856403098711121722383113" +
	"62229893423380308135336276614282806444486645238749" +
	"30358907296290491560440772390713810515859307960866" +
	"70172427121883998797908792274921901699720888093776" +
	"65727333001053367881220235421809751254540594752243" +
	"52584907711670556013604839586446706324415722155397" +
	"53697817977846174064955149290862569321978468622482" +
	"83972241375657056057490261407972968652414535100474" +
	"82166370484403199890008895243450658541227588666881" +
	"16427171479924442928230863465674813919123162824586" +
	"17866458359124566529476545682848912883142607690042" +
	"24219022671055626321111109370544217506941658960408" +
	"07198403850962455444362981230987879927244284909188" +
	"84580156166097919133875499200524063689912560717606" +
	"05886116467109405077541002256983155200055935729725" +
	"71636269561882670428252483600823257530420752963450"
                                                  //> thousandDigits  : java.lang.String = 73167176531330624919225119674426574742
                                                  //| 355349194934969835203127745063262395783180169848018694788518438586156078911
                                                  //| 294949545950173795833195285320880551112540698747158523863050715693290963295
                                                  //| 227443043557668966489504452445231617318564030987111217223831136222989342338
                                                  //| 030813533627661428280644448664523874930358907296290491560440772390713810515
                                                  //| 859307960866701724271218839987979087922749219016997208880937766572733300105
                                                  //| 336788122023542180975125454059475224352584907711670556013604839586446706324
                                                  //| 415722155397536978179778461740649551492908625693219784686224828397224137565
                                                  //| 705605749026140797296865241453510047482166370484403199890008895243450658541
                                                  //| 227588666881164271714799244429282308634656748139191231628245861786645835912
                                                  //| 456652947654568284891288314260769004224219022671055626321111109370544217506
                                                  //| 941658960408071984038509624554443629812309878799272442849091888458015616609
                                                  //| 791913387549920052406368991256071760605886116467109405077541002256983155200
                                                  //| 05593572972571636269561882670428252483600823257530420752963450
//val thousandDigitNum = BigInt(thousandDigits)

val fiftyDigits = "73167176531330624919225119674426574742355349194934"
                                                  //> fiftyDigits  : java.lang.String = 73167176531330624919225119674426574742355
                                                  //| 349194934
def largest5Consec(digits: String) = {
	val zeroChar = '0'.toInt
	def shell(largest: Int, digits: String): Int = {
		val top5 = digits.take(5).map(_.toInt - zeroChar)
		val prod = top5.product
		//if (prod > largest) println("\tthe largest product is now " + prod)
		if (top5.isEmpty) largest
		else shell(largest.max(prod), digits.tail)
 	}
	shell(0, digits)
}                                                 //> largest5Consec: (digits: String)Int

//largest5Consec(thousandDigits)


// Problem 9
// A Pythagorean triplet is a set of three natural numbers, a < b < c, for which a^2 + b^2 = c^2.
// For example, 3^2 + 4^2 = 9 + 16 = 25 == 5^2.
//
// There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
def pythagoreanTriplet = {
	def findTripletStartingWith(a: Int): Option[Tuple3[Int,Int,Int]] = {
		val aSquared = a*a
		def findB(b: Int): Option[Tuple2[Int,Int]] = {
			if (b == 1000) None
			else {
				val c = Math.sqrt(aSquared + b*b)
				if (a+b+c == 1000) Some((b, c.toInt)) else findB(b + 1)
			}
		}

		if (a == 1000) None
		else findB(a+1) match {
			case None => findTripletStartingWith(a+1)
			case Some((b, c)) => Some((a, b, c))
		}
	}
	findTripletStartingWith(1)
}                                                 //> pythagoreanTriplet: => Option[(Int, Int, Int)]
//pythagoreanTriplet

// Problem 10
// The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
// Find the sum of all the primes below two million.
//def primeSum(upTo: Int): Int = {
//	primes.takeWhile(_ < upTo).sum
	//def sum(primeStr: Stream[Int], total: Int): Int = if (primeStr.head >= upTo) total else sum(primeStr.tail)
	//sum(primes, 0)
//}
//primeSum(2000000)
//val p2m = primesUntil((n, primes) => n >= 10)
val p2m = List(2, 3, 5, 7, 11, 13, 17, 19)        //> p2m  : List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19)
//	primesUntil((n, primes) => n >= 2000000)
p2m.length                                        //> res4: Int = 8
p2m.last                                          //> res5: Int = 19
p2m.sum                                           //> res6: Int = 77

def primeBitSetUpTo(top: Int) = {
	def check(num: Int, bits: Seq[Boolean], sum: BigInt): Seq[Boolean] = {
		val idx = num - 2
		println(num + ": bits is " + bits)
		//val sqrt = Math.sqrt(num)
		val newBits = for (bitIdx <- 0 to top-1) yield {
			//println("index is " + bitIdx)
			bits(bitIdx) || (num % (bitIdx+2) != 0)
		}
		check(num+1, newBits, sum + num)
	}
	val primesBitSet = Seq.fill(top)(false)
	check(2, primesBitSet, 0)
}                                                 //> primeBitSetUpTo: (top: Int)Seq[Boolean]
primeBitSetUpTo(10)//2000000)                     //> 2: bits is List(false, false, false, false, false, false, false, false, fa
                                                  //| lse, false)
                                                  //| 3: bits is Vector(false, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 4: bits is Vector(true, true, true, true, true, true, true, true, true, tr
                                                  //| ue)
                                                  //| 5: bits is Vector(true, true, true, true, true, true, true, true, true, tr
                                                  //| ue)
                                                  //| 6: bits is Vector(true, true, true, true, true, true, true, true, true, tr
                                                  //| ue)
                                                  //| 7: bits is Vector(true, true, true, true, true, true, true, true, true, tr
                                                  //| ue)
                                                  //| 8: bits is Vector(true, true, true, true, true, true, true, true, true, tr
                                                  //| ue)
                                                  //| 9: bits is Vector(true, true, true, true, true, true, true, true, true, tr
                                                  //| ue)
                                                  //| 10: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 11: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 12: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 13: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 14: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 15: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 16: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 17: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 18: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 19: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 20: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 21: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 22: bits is Vector(true, true, true, true, true, true, true, true, true, t
                                                  //| rue)
                                                  //| 23: bits is Vector(true, true, true, tru
                                                  //| Output exceeds cutoff limit.\
true
}