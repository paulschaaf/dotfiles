package Expressions

trait Expr {
  /*
  	Add case classes Var for variables x and Prod for products x * y as discussed previously.
	Change your show function so that it also deals with products. Pay attention you get operator
	precedence right but to use as few parentheses as possible.
		
	Example:
	Sum(Prod(2, Var("x")), Var("y"))     should print as "2 * x + y"
			but
	Prod(Sum(2, Var("x")), Var("y"))     should print as "(2 + x) * y"
   */

  def show: String = this match {
    case Var(v) => v
    case Number(n) => n.toString
    case Sum(l, r) => l.show + " + " + r.show
    case Prod(s1: Sum, s2: Sum) => "(" + s1.show + ") * (" + s2.show + ")"
    case Prod(s: Sum,  r) => "(" + s.show + ") * " + r.show
    case Prod(l, s: Sum) => l.show + " * (" + s.show + ")"
    case Prod(l, r) => l.show + " * " + r.show
    case _ => "unkn!"
  }
}

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(v: String) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
	                                                  
