package model


class CalcStack(var stack: List[Any]) {
  type StackElem = Any
  
  // primitives
  def head             = stack.head
  def ::(elem: StackElem)    = stack = (elem :: stack)  // aka 'push'
  
  // def push(elem: StackElem)  = elem :: this
  def pop              = {
	val answer = stack.head
    stack = (stack.tail)
    answer
  }

  def drop(n: Int = 1) = CalcStack(stack.drop(n))
  def empty            = CalcStack(List())
  
  def dup              = head :: this
  def length           = stack.length
  
  def top(n: Int)      = stack.take(n)
  
  def transform2[A,B, C](f: (A, B) => C) = {
    f(pop, pop) :: this
  }
}

object CalcStack {
  def apply(stack: List[Any]) = new CalcStack(stack)
}