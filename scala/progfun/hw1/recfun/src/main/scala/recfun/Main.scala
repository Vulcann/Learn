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
    if (c > r) 0
    if (c == r) 1
    if (c == 0) 1
    pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var openParenNum = 0
    var charsVar = chars
    while (charsVar.nonEmpty) {
      if ( charsVar.head == '(' ) openParenNum += 1
      else if ( charsVar.head == ')') openParenNum -= 1
      charsVar = charsVar.tail
      if (openParenNum < 0) false;
    }
    if (openParenNum == 0) true
    else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if ( coins.isEmpty ) 0
    else if ( money < 0 ) 0
    else if ( money == 0 ) 1
    else countChange(money, coins.tail) + countChange(money-coins.head, coins)
  }
}

