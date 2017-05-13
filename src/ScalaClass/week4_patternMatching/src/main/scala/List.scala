package week4

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int): T
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
  def nth(n: Int) = throw new IndexOutOfBoundsException
}

//object Nil {
//  def apply[T]() = new Nil
//}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  def nth(n: Int): T = {
    if (n < 0) throw new IndexOutOfBoundsException
    if (n == 0) head
    else tail.nth(n - 1)
  }
}

object List {
  // List(1, 2) === List.apply(1, 2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T]() = new Nil
}
