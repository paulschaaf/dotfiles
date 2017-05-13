object worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  var a = new Rational(1, 2)                      //> a  : Rational = 1/2
  var b = new Rational(2, 3)                      //> b  : Rational = 2/3
  a.add(b)                                        //> res0: Rational = 7/6
  b.add(a)                                        //> res1: Rational = 7/6
  a.neg                                           //> res2: Rational = 1/-2
  a.sub(b)                                        //> res3: Rational = 1/-6
  b.sub(a)                                        //> res4: Rational = 1/6
  
  val x = new Rational(1,3)                       //> x  : Rational = 1/3
  val y = new Rational(5,7)                       //> y  : Rational = 5/7
  val z = new Rational(3,2)                       //> z  : Rational = 3/2
                                                  
  x.sub(y).sub(z)                                 //> res5: Rational = -79/42
  x.less(y)                                       //> res6: Boolean = true
  z.max(y)                                        //> res7: Rational = 3/2
  //new Rational(1, 0)
}

class Rational(x: Int, y: Int) {
	require(y != 0, "Denominator must be nonzero!")
	
	def this(x: Int) = this(x, 1)
	
	private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
	
	val numer = x / gcd(x, y)
	val denom = y / gcd(x, y)
	
	def add(that: Rational): Rational = {
		new Rational(
			numer * that.denom + denom * that.numer,
			denom * that.denom
		)
	}
	def sub(that: Rational) = add(that.neg)
	
	def less(that: Rational) = numer * that.denom < that.numer * denom
	def max(that: Rational)  = if (less(that)) that else this
	
	override def toString = numer + "/" + denom
	
	def neg: Rational = new Rational(-numer, denom)
}