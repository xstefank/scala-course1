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
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) {
        1
      }else {
        pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def processChar(parenthesses: Int, string: List[Char]): Boolean = {
        if (string.isEmpty) {
          parenthesses == 0
        } else {
          val character = string.head
          val change = {
            if (character == '(') parenthesses + 1
            else if (character == ')') parenthesses - 1
            else parenthesses
          }

          if (parenthesses >= 0) processChar(change, string.tail)
          else false
        }
      }

      processChar(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        1
      } else if (!coins.isEmpty && money > 0) {
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      } else {
        0
      }
    }
  }
