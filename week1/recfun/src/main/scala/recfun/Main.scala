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
      def factorial(n: Int): Int = {
        def loop(acc: Int, n: Int): Int =
          if (n == 0) acc
          else loop(acc * n, n - 1);
        loop(1, n)
      }

      factorial(r) / (factorial(r - c) * factorial(c))
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      import scala.collection.mutable.Stack

      def processChar(parenthesses: Stack[Char], string: List[Char]): Boolean = {
        //if we are on the end on the string the stack must be empty
        if (string.isEmpty) {
          parenthesses.isEmpty
        } else {

          //else process the first character
          //if it is '(' add it to stack
          //if it is ')' remove one element from the stack
          val character = string.head;
          if (character.equals('(')) {
            parenthesses.push('(');
          } else if (character.equals(')')) {
            //if the stack is empty there is no matching '('
            if (parenthesses.isEmpty) {
              return false
            } else {
              parenthesses.pop();
            }
          }
          processChar(parenthesses, string.tail);
        }
      }

      processChar(Stack[Char](), chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        //we made the exact number from coins - valid option
        1
      } else if (!coins.isEmpty && money > 0) {
        //still have some money and some coins left
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      } else {
        0
      }
    }
  }
