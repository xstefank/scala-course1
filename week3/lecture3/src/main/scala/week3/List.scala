package week3

/**
  * Created by mstefank on 7/25/16.
  */
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def find(x: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false

  override def find(x: Int): T = {
    if (x > 0)
      tail.find(x - 1)
    else if (x == 0)
      this.head
    else
      throw new IndexOutOfBoundsException("Invalid index")
  }
}

class Nil[T] extends List[T] {
  override def isEmpty = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def find(x: Int): T = throw new IndexOutOfBoundsException("Invalid index")
}
