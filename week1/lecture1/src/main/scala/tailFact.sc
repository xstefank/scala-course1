object tailFact {
  def factorialBad(x: Int): Int = {
    if (x == 0) 1 else x * factorialBad(x - 1)
  }

  factorialBad(4)

  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1);
    loop(1, n)
  }

  factorial(4)
}