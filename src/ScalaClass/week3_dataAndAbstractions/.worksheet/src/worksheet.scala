object worksheet {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(62); 
  println("Welcome to the Scala worksheet");$skip(29); 
  var a = new Rational(1, 2);System.out.println("""a  : Rational = """ + $show(a ));$skip(29); 
  var b = new Rational(2, 3);System.out.println("""b  : Rational = """ + $show(b ));$skip(11); val res$0 = 
  a.add(b);System.out.println("""res0: Rational = """ + $show(res$0));$skip(11); val res$1 = 
  b.add(a);System.out.println("""res1: Rational = """ + $show(res$1));$skip(8); val res$2 = 
  a.neg;System.out.println("""res2: Rational = """ + $show(res$2));$skip(11); val res$3 = 
  a.sub(b);System.out.println("""res3: Rational = """ + $show(res$3));$skip(11); val res$4 = 
  b.sub(a);System.out.println("""res4: Rational = """ + $show(res$4));$skip(31); 
  
  val x = new Rational(1,3);System.out.println("""x  : Rational = """ + $show(x ));$skip(28); 
  val y = new Rational(5,7);System.out.println("""y  : Rational = """ + $show(y ));$skip(28); 
  val z = new Rational(3,2);System.out.println("""z  : Rational = """ + $show(z ));$skip(69); val res$5 = 
                                                  
  x.sub(y).sub(z);System.out.println("""res5: Rational = """ + $show(res$5));$skip(12); val res$6 = 
  x.less(y);System.out.println("""res6: Boolean = """ + $show(res$6));$skip(11); val res$7 = 
  z.max(y);System.out.println("""res7: Rational = """ + $show(res$7))}
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