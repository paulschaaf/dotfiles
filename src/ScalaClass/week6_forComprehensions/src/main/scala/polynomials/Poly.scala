package polynomials

case class Poly(terms0: Map[Int, Double]) {

  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

  def +(other: Poly) = new Poly(terms ++ (other.terms map adjust))

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }

  def ++(other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
    val (exp, coeff) = term
    //terms.updated(exp, coeff + terms(exp))
    terms + (exp ->(coeff + terms(exp)))
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse)
      yield coeff + "x^" + exp) mkString " + "

}