// I: Function, A: Data
trait Iter[I,A] {
  def getValue(i: I): Option[A]
  def getNext(i: I): I
}

trait Iterable[R,A] {
  type itr
  def iterIF: Iter[itr,A]
  def iter(a: R): itr
}

trait ListIF[L,A] {
  def empty: L
  def head(l:L): Option[A]
  def tail(l:L): L
  def cons(a: A, l: L): L
  def append(l1: L, l2: L): L
}

trait TreeIF[T,A] {
  def empty: T
  def node(a: A, l: T, r: T): T
  def head(t: T): Option[A]
  def left(t: T): T
  def right(t: T): T
}

def sumElements[I](xs: I)(implicit IT: Iter[I,Int]): Int = {
  IT.getValue(xs) match {
    case None => 0
    case Some(n) => n + sumElements(IT.getNext(xs))
  }
}

def sumElements2[R](xs: R)(implicit ITR: Iterable[R,Int]): Int = {
  sumElements(ITR.iter(xs))(ITR.iterIF)
}

def printElements[I](xs: I)(implicit IT: Iter[I,Int]): Unit = {
  IT.getValue(xs) match {
    case None =>
    case Some(n) => println(n); printElements(IT.getNext(xs))
  }
}

def printElements2[R](xs:R)(implicit ITR: Iterable[R,Int]): Unit = {
  printElements(ITR.iter(xs))(ITR.iterIF)
}

implicit def listIter[A] : Iter[List[A],A] = {
  new Iter[List[A],A] {
    override def getValue(i: List[A]): Option[A] = i.headOption
    override def getNext(i: List[A]): List[A] = i.tail
  }
}

implicit def listIF[A]: ListIF[List[A],A] =
  new ListIF[List[A],A] {
    override def empty: List[A] = Nil

    override def head(l: List[A]): Option[A] = l.headOption

    override def tail(l: List[A]): List[A] = l.tail

    override def cons(a: A, l: List[A]): List[A] = a :: l

    override def append(l1: List[A], l2: List[A]): List[A] = l1 ::: l2
  }

sealed abstract class MyTree[A]
case class Empty[A]() extends MyTree[A]
case class Node[A](value: A, left: MyTree[A], right: MyTree[A]) extends MyTree[A]

implicit def treeIF[A]: TreeIF[MyTree[A],A] = new TreeIF[MyTree[A],A] {
  override def empty: MyTree[A] = Empty()
  override def head(t: MyTree[A]): Option[A] = t match {
    case Empty() => None
    case Node(v, l, r) => Some(v)
  }
  override def node(a: A, l: MyTree[A], r: MyTree[A]): MyTree[A] = Node(a,l,r)
  override def left(t: MyTree[A]): MyTree[A] = t match {
    case Empty() => t
    case Node(v,l,r) => l
  }
  override def right(t: MyTree[A]): MyTree[A] = t match {
    case Empty() => t
    case Node(v,l,r) => r
  }
}

def treeIterable[L,A](implicit IF: ListIF[L,A], IT: Iter[L,A]): Iterable[MyTree[A],A] = new Iterable[MyTree[A],A] {
  override type itr = L
  override def iter(a: MyTree[A]): L = a match {
    case Empty() => IF.empty
    case Node(v,l,r) =>IF.cons(v,IF.append(iter(l),iter(r)))
  }
  override def iterIF: Iter[L, A] = IT
}

implicit def treeIterableList[A] = treeIterable[List[A],A]

def treeTest[T](implicit TI: TreeIF[T,Int], ITR: Iterable[T,Int]): Unit = {
  val t: T = TI.node(3, TI.node(5,TI.empty,TI.empty),TI.node(6,TI.empty,TI.empty))
  println(sumElements2(t))
  printElements2(t)
}

treeTest[MyTree[Int]]
