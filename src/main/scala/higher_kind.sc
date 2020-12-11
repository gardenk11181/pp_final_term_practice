// higher-kind-type : type => type
trait Iter[I[_]] {
  def getValue[A](a: I[A]): Option[A]
  def getNext[A](a: I[A]): I[A]
}

trait Iterable[R[_]] {
  type Itr[_]
  def iter[A](a: R[A]): Itr[A]
  def iterIF: Iter[Itr]
}

trait ListIF[I[_]] {
  def empty[A]: I[A]
  def head[A](l: I[A]): Option[A]
  def tail[A](l: I[A]): I[A]
  def cons[A](v: A, l: I[A]): I[A]
  def append[A](l1: I[A], l2: I[A]): I[A]
}

trait TreeIF[T[_]] {
  def empty[A]: T[A]
  def node[A](v: A, l: T[A], r: T[A]): T[A]
  def head[A](t: T[A]): Option[A]
  def left[A](t: T[A]): T[A]
  def right[A](t: T[A]): T[A]
}

def sumElements[I[_]](xs: I[Int])(implicit IT: Iter[I]): Int = {
  IT.getValue(xs) match {
    case None => 0
    case Some(n) => n + sumElements(IT.getNext(xs))
  }
}

def printElements[I[_],A](xs: I[A])(implicit IT: Iter[I]): Unit = {
  IT.getValue(xs) match {
    case None =>
    case Some(n) => println(n); printElements(IT.getNext(xs))
  }
}

def sumElements2[R[_]](xs: R[Int])(implicit ITR: Iterable[R]): Int = {
  sumElements(ITR.iter(xs))(ITR.iterIF)
}

def printElements2[R[_],A](xs: R[A])(implicit ITR: Iterable[R]): Unit = {
  printElements(ITR.iter(xs))(ITR.iterIF)
}

def testList[L[_]](implicit LI: ListIF[L], iter: Iter[L]): Unit = {
  val l: L[Int] = LI.cons(3,LI.cons(5,LI.cons(2,LI.cons(1,LI.empty))))
  println(sumElements(l))
  printElements(l)
}

def testTree[T[_]: Iterable](implicit TI: TreeIF[T]): Unit = {
  val t = TI.node(3,TI.node(4,TI.empty,TI.empty),TI.node(2,TI.empty,TI.empty))
  println(sumElements2(t))
  printElements2(t)
}

implicit val listIter: Iter[List] =
  new Iter[List] {
    def getValue[A](a: List[A]): Option[A] = a.headOption
    def getNext[A](a: List[A]): List[A] = a.tail
  }

implicit val listIF: ListIF[List] = new ListIF[List] {
  override def empty[A]: List[A] = Nil
  override def head[A](l: List[A]): Option[A] = l.headOption
  override def tail[A](l: List[A]): List[A] = l.tail
  override def cons[A](v: A, l: List[A]): List[A] = v::l
  override def append[A](l1: List[A], l2: List[A]): List[A] = l1:::l2
}

sealed abstract class MyTree[A]
case class Empty[A]() extends MyTree[A]
case class Node[A](value: A,lt: MyTree[A],rt: MyTree[A]) extends MyTree[A]

implicit def treeIterable[L[_]](implicit IF: ListIF[L], IT:Iter[L]): Iterable[MyTree] = new Iterable[MyTree] {
  override type Itr[A] = L[A]
  override def iter[A](a: MyTree[A]): L[A] = a match {
    case Empty() => IF.empty
    case Node(v,l,r) => IF.cons(v,IF.append(iter(l),iter(r)))
  }
  override val iterIF: Iter[L] = IT
}

implicit val treeIF: TreeIF[MyTree] = new TreeIF[MyTree] {
  override def empty[A]: MyTree[A] = Empty()
  override def head[A](t: MyTree[A]): Option[A] = t match {
    case Empty() => None
    case Node(hd,lt,rt) => Option(hd)
  }
  override def node[A](v: A, l: MyTree[A], r: MyTree[A]): MyTree[A] = Node(v,l,r)
  override def left[A](t: MyTree[A]): MyTree[A] = t match {
    case Empty() => t
    case Node(_,l,_) => l
  }
  override def right[A](t: MyTree[A]): MyTree[A] = t match {
    case Empty() => t
    case Node(_,_,r) => r
  }
}

testList[List]
testTree[MyTree]