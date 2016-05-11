import java.util.Date
import scala.annotation.tailrec

object Euler_11 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

def time(f: => Unit) {
  val t1 = System.currentTimeMillis()
  f
  val t2 = System.currentTimeMillis()
  println((t2 - t1).asInstanceOf[Float]+" msecs")
}                                                 //> time: (f: => Unit)Unit
/*
  bitset<1000000> Sieve;
  __int64 sum = 0;
        
  Sieve.flip();    // Set all bits to 1
  Sieve[0].flip();  // Set 0 and 1 to not prime
  Sieve[1].flip();

  // Check all nos from 2 to 1 million
  for(long i = 2; i < 1000000; ++i) {
    if( ! Sieve ) continue // If marked not prime return to head of loop
    else {
      // Set all multiples as not prime
      for(long j = 2*i; j < 1000000; j += i)
        Sieve[j] = 0;
    }
  }

  for(long i = 2; i < 1000000; ++i) if( Sieve ) sum += i // Add all nos with bit set

  cout << "\nThe sum is : "  << sum << endl;
*/

def toBinary(i: Int, digits: Int = 8) = String.
		format("%" + digits + "s", i.toBinaryString).
		replace(' ', '0')                 //> toBinary: (i: Int, digits: Int)java.lang.String
    
// get the number of bits that we need
//val bitField = twoToThe(cutoff) //(1 to cutoff).foldLeft(BigInt(1))((prod, each) => prod*2)
case class BitMap(map: BigInt) extends BigInt(map.bigInteger) {
	override val toString =
			if (false && map > 512) "%#x" format map  // show in hexadecimal
			else {
				val bits = toBinary(map.toInt)//.splitAt(4)
				map.toString + " {" + bits + "}"
			}
	override def &(other: BigInt) = BitMap(map & other)
	override def |(other: BigInt) = BitMap(map | other)
	override def ^(other: BigInt) = BitMap(map ^ other)
	def at(n: Int) = (map >> n-1) & 1
	def bitIsClear(n: Int) = (this at n) == 0
	def bitIsSet(n: Int) = ! bitIsClear(n)
}

def twoToThe(exp: Int) = BigInt(1) << exp         //> twoToThe: (exp: Int)scala.math.BigInt

def sumOfPrimesBelow(cutoff: Int) = {
	def primeBits(n: Int, sum: BigInt, primeBitMap: BitMap): BigInt = {
		//printf("primeBits(%d, %d, %s)\n", n, sum, primeBitMap.toString)
		if (n > cutoff) sum
		else {
			if (primeBitMap bitIsClear n) {
				val multiplesRange = (2*n) to cutoff by n
				//println("  " + n + " is prime, marking all multiples")// in " + multiplesRange)
				val factorsMap = multiplesRange.   // for each multiple of iter
						foldLeft(BigInt(0))((factMap: BigInt, factor: Int) => factMap | twoToThe(factor-1)) // set that bit, thereby marking the number as composite
				primeBits(n + 1, sum + n, primeBitMap | factorsMap)
			}
			else {
				//println("  " + n + " is not prime")
				primeBits(n + 1, sum, primeBitMap) // if this bit is already set n is composite
			}
		}
	}
	//println("-----")
	primeBits(2, 0, BitMap(BigInt(1) << cutoff))
}                                                 //> sumOfPrimesBelow: (cutoff: Int)BigInt

//sumOfPrimesBelow(10)

val before = new Date                             //> before  : java.util.Date = Wed Jan 30 12:41:42 PST 2013
//sumOfPrimesBelow(2000000)
new Date                                          //> res0: java.util.Date = Wed Jan 30 12:41:42 PST 2013

// Problem 11
// In the 2020 grid below, four numbers along a diagonal line have been marked with arrows.
/*                       |
                         v
08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70  // <--
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

 The product of these numbers is 26  63  78  14 = 1788696.

 What is the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in the 2020 grid?
*/
type Grid = List[List[Int]]

val bigGrid = List(
    List( 8,  2, 22, 97, 38, 15,  0, 40,  0, 75,  4,  5,  7, 78, 52, 12, 50, 77, 91,  8),
    List(49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48,  4, 56, 62,  0),
    List(81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30,  3, 49, 13, 36, 65),
    List(52, 70, 95, 23,  4, 60, 11, 42, 69, 24, 68, 56,  1, 32, 56, 71, 37,  2, 36, 91),
    List(22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80),
    List(24, 47, 32, 60, 99,  3, 45,  2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50),
    List(32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70),
    List(67, 26, 20, 68,  2, 62, 12, 20, 95, 63, 94, 39, 63,  8, 40, 91, 66, 49, 94, 21),
    List(24, 55, 58,  5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72),
    List(21, 36, 23,  9, 75,  0, 76, 44, 20, 45, 35, 14,  0, 61, 33, 97, 34, 31, 33, 95),
    List(78, 17, 53, 28, 22, 75, 31, 67, 15, 94,  3, 80,  4, 62, 16, 14,  9, 53, 56, 92),
    List(16, 39,  5, 42, 96, 35, 31, 47, 55, 58, 88, 24,  0, 17, 54, 24, 36, 29, 85, 57),
    List(86, 56,  0, 48, 35, 71, 89,  7,  5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58),
    List(19, 80, 81, 68,  5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77,  4, 89, 55, 40),
    List( 4, 52,  8, 83, 97, 35, 99, 16,  7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66),
    List(88, 36, 68, 87, 57, 62, 20, 72,  3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69),
    List( 4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18,  8, 46, 29, 32, 40, 62, 76, 36),
    List(20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74,  4, 36, 16),
    List(20, 73, 35, 29, 78, 31, 90,  1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57,  5, 54),
    List( 1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52,  1, 89, 19, 67, 48)
)                                                 //> bigGrid  : List[List[Int]] = List(List(8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 
                                                  //| 4, 5, 7, 78, 52, 12, 50, 77, 91, 8), List(49, 49, 99, 40, 17, 81, 18, 57, 6
                                                  //| 0, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0), List(81, 49, 31, 73, 55, 79, 
                                                  //| 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65), List(52, 70, 95, 23
                                                  //| , 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91), List(22, 31
                                                  //| , 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80), 
                                                  //| List(24, 47, 32, 60, 99, 3, 45, 2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 
                                                  //| 12, 50), List(32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 6
                                                  //| 6, 18, 38, 64, 70), List(67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63,
                                                  //|  8, 40, 91, 66, 49, 94, 21), List(24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78
                                                  //| , 78, 96, 83, 14, 88, 34, 89, 63, 72), List(21, 36, 23, 9, 75, 0, 76, 44, 2
                                                  //| 0, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95), List(78, 17, 53, 28, 22, 75,
                                                  //|  31, 67, 15, 94, 3, 80, 4, 62, 16, 14, 9, 53, 56, 92), List(16, 39, 5, 42, 
                                                  //| 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57), List(86, 56
                                                  //| , 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58), Lis
                                                  //| t(19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55,
                                                  //|  40), List(4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33,
                                                  //|  27, 98, 66), List(88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 1
                                                  //| 2, 32, 63, 93, 53, 69), List(4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18,
                                                  //|  8, 46, 29, 32, 40, 62, 76, 36), List(20, 69, 36, 41, 72, 30, 23, 88, 34, 6
                                                  //| 2, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16), List(20, 73, 35, 29, 78, 31, 90,
                                                  //|  1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54), List(1, 70, 54, 71, 83,
                                                  //|  51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48))

val littleDimensions = 7                          //> littleDimensions  : Int = 7
val littleGrid = bigGrid.take(littleDimensions).
												 map(_.take(littleDimensions))
                                                  //> littleGrid  : List[List[Int]] = List(List(8, 2, 22, 97, 38, 15, 0), List(49
                                                  //| , 49, 99, 40, 17, 81, 18), List(81, 49, 31, 73, 55, 79, 14), List(52, 70, 9
                                                  //| 5, 23, 4, 60, 11), List(22, 31, 16, 71, 51, 67, 63), List(24, 47, 32, 60, 9
                                                  //| 9, 3, 45), List(32, 98, 81, 28, 64, 23, 67))
def grid4TuplesList(aGrid: Grid): List[List[Int]] = {
	def recurseGrid(subGrid: Grid): List[List[Int]] = subGrid match {
		case  _ ::  _ ::  _ :: Nil          => List()            // since less than 4 rows remain we're done!
		case (_ ::  _ ::  _ :: Nil) :: _    => grid4TuplesList(aGrid.tail) // less than 4 columns, so recurse on this grid minus the first row
		case (a0 :: a1 :: a2 :: a3 :: _) ::
				 (b0 :: b1 :: b2 :: b3 :: _) ::
				 (c0 :: c1 :: c2 :: c3 :: _) ::
				 (d0 ::  _ ::  _ :: d3 :: _) ::
				 _ => {
			List(a0, a1, a2, a3) ::            // - (first row)
			List(a0, b0, c0, d0) ::            // | (first column)
			List(a0, b1, c2, d3) ::            // \ (diagonal starting from a0)
			List(a3, b2, c1, d0) ::            // / (diagonal starting from a3)
			recurseGrid(subGrid.map(_.tail)) // recurse on all but the first column
		}
	}
	recurseGrid(aGrid)
}                                                 //> grid4TuplesList: (aGrid: Euler_11.Grid)List[List[Int]]

def grid4TuplesStream(aGrid: Grid): Stream[List[Int]] = {
	def recurseGrid(subGrid: Grid): Stream[List[Int]] = subGrid match {
		case  _ ::  _ ::  _ :: Nil          => Stream.empty            // since less than 4 rows remain we're done!
		case (_ ::  _ ::  _ :: Nil) :: _    => grid4TuplesStream(aGrid.tail) // less than 4 columns, so recurse on this grid minus the first row
		case (a0 :: a1 :: a2 :: a3 :: _) ::
				 (b0 :: b1 :: b2 :: b3 :: _) ::
				 (c0 :: c1 :: c2 :: c3 :: _) ::
				 (d0 ::  _ ::  _ :: d3 :: _) ::
				 _ => {
			List(a0, a1, a2, a3) #::            // - (first row)
			List(a0, b0, c0, d0) #::            // | (first column)
			List(a0, b1, c2, d3) #::            // \ (diagonal starting from a0)
			List(a3, b2, c1, d0) #::            // / (diagonal starting from a3)
			recurseGrid(subGrid.map(_.tail)) // recurse on all but the first column
		}
	}
	recurseGrid(aGrid)
}                                                 //> grid4TuplesStream: (aGrid: Euler_11.Grid)Stream[List[Int]]

val tuples = grid4TuplesStream(bigGrid)           //> tuples  : Stream[List[Int]] = Stream(List(8, 2, 22, 97), ?)

time {
	//tuples.foldRight(0)((tuple, largest) => tuple.product.max(largest))
	tuples.foldRight(0)(_.product.max(_))
}                                                 //> 73.0 msecs
time {
	tuples.foldLeft(0)((largest, tuple) => largest.max(tuple.product))
}                                                 //> 11.0 msecs

time {
	tuples.foldRight(List(0))((tuple, largest) => {
		val prod = tuple.product
		if (prod > largest.head) prod :: tuple
		else largest
	})
}                                                 //> 4.0 msecs

// Alternative implementation found online
    //method that returns only valid lines
    def toLine(x: Int, y: Int, dirX: Int, dirY: Int) = {
      if (dirX != 0 && dirY != 0) {
        val lineNumbers = {
          def inBounds(i: Int) = i >= 0 && i < 20
          for ((col, row) <- 0 until 4 map {i => (x + i * dirX, y + i * dirY)}
               if inBounds(col) && inBounds(row))
          yield (bigGrid(row)(col))
        }
        if (lineNumbers.size == 4) Some(lineNumbers)
        else None
      }
      else None
    }                                             //> toLine: (x: Int, y: Int, dirX: Int, dirY: Int)Option[scala.collection.immut
                                                  //| able.IndexedSeq[Int]]

time {
    //loop over everything, collect the products
    (for (x <- 0 until 20;
         y <- 0 until 20;
         xDirection <- List(-1, 0, 1);
         yDirection <- List(-1, 0, 1);
         line <- toLine(x, y, xDirection, yDirection)
    ) yield line.product).max
}                                                 //> 235.0 msecs

// Problem 12
/*
		The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.

		The first ten terms would be: 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
		
		Let us list the factors of the first seven triangle numbers:
		
		 1: 1
		 3: 1,3
		 6: 1,2,3,6
		10: 1,2,5,10
		15: 1,3,5,15
		21: 1,3,7,21
		28: 1,2,4,7,14,28
		
		We can see that 28 is the first triangle number to have over five divisors.
		
		What is the value of the first triangle number to have over five hundred divisors?
*/

def primeNumbersUpTo(limit: Double): BitMap = {
//	@tailrec
//	def sieveCheckFactors(num: Int, primesSoFar: Stream[Int]): Stream[Int] = {
// 	  val sqrt = scala.Math.sqrt(num.toDouble)
// 	  val newPrimes = if (primesSoFar.takeWhile(_ <= sqrt).exists(num % _ == 0)) primesSoFar
// 		else primesSoFar :+ num
// 		sieveCheckFactors(num + 2, primesSoFar :+ num)
//	}
//	sieveCheckFactors(3, List(2))
/*	@tailrec
	def nextPrime(num: Int): Stream[Int] = {
 	  val sqrt = scala.Math.sqrt(num.toDouble)
		if (primeNumbers.takeWhile(_ <= sqrt).exists(num % _ == 0)) nextPrime(num + 2)
		else num #:: nextPrime(num + 2)
	}
	2 #:: nextPrime(3)
	*/
	
	//(0d to Math.log(limit)/Math.log(2))
	
	null
}                                                 //> primeNumbersUpTo: (limit: Double)Euler_11.BitMap
val triangle = {
	def tri(n: Int, lastTri: Int): Stream[Int] = {
		val thisTri = lastTri + n
		thisTri #:: tri(n+1, thisTri)
	}
	tri(1, 0)
}                                                 //> triangle  : Stream[Int] = Stream(1, ?)

def tau(n: Int): Int = {4
	
}                                                 //> tau: (n: Int)Int


1                                                 //> res1: Int(1) = 1
}