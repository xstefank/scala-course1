object WorkSheet {
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)

  removeAt(1, List('a', 'b', 'c', 'd'))

  def flatten(xs: List[Any]): List[Any] = xs match {
    case Nil => Nil
    case head :: tail => (head match {
      case l: List[Any] => flatten(l)
      case i => List(i)
    }) ::: flatten(tail)
  }

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
}