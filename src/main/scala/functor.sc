trait Functor[F[_]] {
  def map[A,B](f: A=>B)(x: F[A]): F[B]
}

def compose[F[_],A,B,C](g: B=>C)(f: A=>B)(a: F[A])(implicit FT: Functor[F]): F[C] = {
  FT.map(g)(FT.map(f)(a))
}

implicit val ListFunctor: Functor[List] = new Functor[List] {
  override def map[A, B](f: A => B)(x: List[A]): List[B] = x.map(f)
}

sealed abstract class MyTree[A]
case class Empty[A]() extends MyTree[A]
case class Node[A](value: A,lt: MyTree[A],rt: MyTree[A]) extends MyTree[A]

implicit val MyTreeFunctor: Functor[MyTree] = new Functor[MyTree] {
  override def map[A, B](f: A => B)(x: MyTree[A]): MyTree[B] = x match {
    case Empty() => Empty()
    case Node(v,l,r) => Node(f(v),map(f)(l),map(f)(r))
  }
}

compose((x: Int)=>x*x)((x:Int)=>x+x)(List(1,2,3))
val t: MyTree[Int] = Node(3,Node(4,Empty(),Empty()), Node(2,Empty(),Empty()))
compose((x:Int)=>x*x)((x:Int)=>x+x)(t)

// Even Higher Kind
// Iter : (* => *) => *
abstract class Iter[I[_]] {
  def getValue[A](a: I[A]): Option[A]
  def getNext[A](a: I[A]): I[A]
}

// Foo : ((* => *)=>*)=>*
abstract class Foo[I[_[_]]] {
  def get: I[List]
}

def f(x: Foo[Iter]): Iter[List] = x.get