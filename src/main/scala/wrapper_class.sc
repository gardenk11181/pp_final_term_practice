abstract class Iter[A] {
  def getValue: Option[A]
  def getNext: Iter[A]
}
abstract class Iterable[A] {
  def iter: Iter[A]
}

class ListIter[A](val list: List[A]) extends Iter[A] {
  override def getValue: Option[A] = list.headOption
  override def getNext: Iter[A] = new ListIter[A](list.tail)
}

def sumElements[A](f: A=>Int)(xs: Iter[A]): Int = {
  xs.getValue match {
    case None => 0
    case Some(n) => f(n)+sumElements(f)(xs.getNext)
  }
}
def sumElementsGen[A](f: A=>Int)(xs: Iterable[A]): Int = {
  sumElements(f)(xs.iter)
}

sumElements((x:Int)=>x)(new ListIter(List(1,2,3,4)))

sealed abstract class MyTree[A] extends Iterable[A] {
  override def iter : ListIter[A]
}
case class Empty[A]() extends MyTree[A] {
  override val iter : ListIter[A] = new ListIter[A](Nil)
}
case class Node[A](value: A, lt: MyTree[A], rt: MyTree[A]) extends MyTree[A] {
  override val iter : ListIter[A] = new ListIter[A](value::(lt.iter.list++rt.iter.list))
}

val t: MyTree[Int] = Node(3,Node(4,Node(2,Empty(),Empty()),Node(3,Empty(),Empty())),Node(5,Empty(),Empty()))

sumElementsGen((x:Int)=>x)(t)