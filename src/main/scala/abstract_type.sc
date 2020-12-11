abstract class Iterable[A] {
  type iter_t
  def iter: iter_t
  def getValue(i: iter_t): Option[A]
  def getNext(i: iter_t): iter_t
}

def sumElements[A](f:A=>Int)(xs: Iterable[A]): Int = {
  def sumElementsIter(i: xs.iter_t) : Int = {
    xs.getValue(i) match {
      case None => 0
      case Some(n) => f(n) + sumElementsIter(xs.getNext(i))
    }
  }
  sumElementsIter(xs.iter)
}

sealed abstract class MyTree[A] extends Iterable[A] {
  type iter_t = List[A]
  def getValue(i: List[A]): Option[A] = i.headOption
  def getNext(i: List[A]): List[A] = i.tail
}
case class Empty[A]() extends MyTree[A] {
  val iter : List[A] = Nil
}
case class Node[A](value: A, lt: MyTree[A], rt: MyTree[A]) extends MyTree[A] {
  val iter : List[A] = value::(lt.iter++rt.iter)
}

