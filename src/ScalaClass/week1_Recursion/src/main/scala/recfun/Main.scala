package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0 || c > r) 0 // out-of-bounds coordinates return 0
    else if (c == 0 || c == r) 1 // the first and last element of each row is 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   * count++ for each open, count-- for each close, ensure the count is never negative, end count should be 0
   */
  def balance(chars: List[Char]): Boolean = {

    def updateCount(count: Int, lst: List[Char]): Boolean = {
      if (count < 0) false
      else if (lst.isEmpty) count == 0
      else updateCount(
        count + (if (lst.head == '(') 1 else if (lst.head == ')') -1 else 0),
        lst.tail)
    }
    updateCount(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if      (money == 0) 1                          // exact change!
    else if (money <= 0                             // if this would give too much change
    	  || coins.isEmpty) 0                       // or if we ran out of coins
    else countChange(money - coins.head, coins) +   // all of the combinations with at least one of the head coin
    	 countChange(money, coins.tail)             // plus all without any of the head coin
  }
}
