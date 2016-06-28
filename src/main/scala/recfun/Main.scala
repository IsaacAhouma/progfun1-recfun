package recfun

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
  // Constructs Pascal's Triangle
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r <= 1 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
   * Exercise 2
   */

  // Checks if the parentheses in a sequence of characters are balanced
  def balance(chars: List[Char]): Boolean = {

    def checkBalance(chars: List[Char], i: Int): Boolean = {
      if (chars.isEmpty) i == 0
      else
      if (chars.head == '(') checkBalance(chars.tail, i + 1)
      else
      if (chars.head == ')') i > 0 && checkBalance(chars.tail, i - 1)
      else checkBalance(chars.tail, i)
    }

    checkBalance(chars, 0)

  }

  /**
   * Exercise 3
   */
  // Given an amount and a list of possible type of coins available, returns how many different ways change can be returned
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0) 1
    else
        if (money > 0 && !coins.isEmpty)
            countChange(money - coins.head, coins) + countChange(money, coins.tail)
        else 0

    }
  }
