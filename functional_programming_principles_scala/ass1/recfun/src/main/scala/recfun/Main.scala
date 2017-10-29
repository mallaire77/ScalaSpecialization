package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 100) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(col: Int, row: Int): Int = {
    @tailrec
    def innerPascal(_col: Int, _row: Int, previousElem: Int): Int =
      if (_col == col)
        previousElem * _row / _col
      else
        innerPascal(_col + 1, _row - 1, previousElem * _row / _col)

    if (col > row)
      0
    else if (col == 0 || row == 0 || col == row)
      1
    else
      innerPascal(1, row, 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def innerBalance(chars: List[Char], parenthesesCount: Int): Boolean =
      if (parenthesesCount < 0)
        false
      else if (chars.isEmpty)
        parenthesesCount == 0
      else
        chars.head match {
          case '(' =>
            innerBalance(chars.tail, parenthesesCount + 1)
          case ')' =>
            innerBalance(chars.tail, parenthesesCount - 1)
          case _ =>
            innerBalance(chars.tail, parenthesesCount)
        }

    if (chars.isEmpty)
      true
    else
      innerBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def innerCountChange(money: Int, coins: List[Int], combinationCount: Int): Int =
      if (money == 0)
        combinationCount + 1
      else if (money < 0 || coins.isEmpty)
        combinationCount
      else
        innerCountChange(money - coins.head, coins, combinationCount) + innerCountChange(money, coins.tail, combinationCount)

    if(money <= 0 || coins.isEmpty)
      0
    else
      innerCountChange(money, coins.sortWith(_ < _), 0)
  }
}
