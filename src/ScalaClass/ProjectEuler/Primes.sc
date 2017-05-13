object Primes {
  println("Welcome to the Scala worksheet")

	def toBinary(i: Int, digits: Int = 8) = String.
			format("%" + digits + "s", i.toBinaryString)//.
			//replace(' ', '0')
	    
	// get the number of bits that we need
	//val bitField = twoToThe(cutoff) //(1 to cutoff).foldLeft(BigInt(1))((prod, each) => prod*2)
	case class BitField(field: BigInt) {
		override val toString = {
				//if (field > 512) "%#x" format field  // show in hexadecimal
				//else {
					val bits = toBinary(field.toInt, 24)//.splitAt(4)
					//field.toString +
					"{" + bits + "}"
				//}
		}
		def ~                = BitField(~field)
		def &(other: BigInt) = BitField(field & other)
		def |(other: BigInt) = BitField(field | other)
		def ^(other: BigInt) = BitField(field ^ other)
		
		def at(n: Int) = (field >> n-1) & 1
		def bitIsClear(n: Int) = (this at n) == 0
		def bitIsSet(n: Int) = ! bitIsClear(n)
		def length = Math.log(field.toDouble) / Math.log(2)
	}

  class Primes(upTo: BigInt) {
		val bits = genPrimes(2, setBit(0, upTo+2)-1)
		
		private def clearBit(num: Int, n: BigInt) = num & ~(setBit(0, n))
		private def setBit(num: Int, n: BigInt)   = num |  (1 << (n - 2))
		private def bit(num: Int, n: BigInt) = {
			val mask = setBit(0, n)
			(num & mask) > 0
		}
		
		def genPrimes(num: Int, soFar: BigInt): Int = {
			//println("  genPrimes(" + num + ", \t" + BitField(soFar) + ")")
			if (num >= upTo) soFar
			else if (bit(soFar, num)) {
				println(num + " is prime")
				val mask = (2*num to upTo by num).foldLeft(0)((composites, multiple) => {
					val nextMask = setBit(composites, multiple)
					//println("  zeroing multiple " + multiple + ": \tnextMask == " + BitField(nextMask))
					//val bit = setBit(0, multiple)
					//println("  zeroing multiple " + multiple + ":\tpattern == ~" + BitField(composites | bit))
					nextMask
				})
				//println("mask is " + BitField(mask))
				genPrimes(num + 1, soFar & (~mask))
			}
			else genPrimes(num + 1, soFar)
		}
		
		def isPrime(num: Int) = bit(bits, num)
		
		override def toString = {
			println(bits.toString)
			val nums = (2 to upTo).foldLeft("(")((str, position) => {
				if (bit(bits, position)) str + position + ", "
				else str
			})
			nums + ")"
		}
  }
  
  val p = new Primes(32)
  p.isPrime(3)

  p.isPrime(7)
  p.isPrime(9)
  p.isPrime(21)
  p.isPrime(23)
  p.isPrime(27)
  p.isPrime(29)
/**/

	// avoid error if last line contains an inline comment
	Nil
}