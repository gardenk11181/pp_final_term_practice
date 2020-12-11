import scala.annotation.tailrec

class A(val a: Int) {
  def this() = this(0)
}
trait B {
  def f(x: Int): Int = x
}
trait C extends A with B {
  def g(x: Int): Int = x+a
}
trait D extends B {
  def h(x: Int): Int = f(x+50)
}
class E extends A(10) with C with D {
  override def f(x: Int): Int = x*a
}

val e = new E

// constructor : post-order traversing

abstract class Iter[A] {
  def getValue: Option[A]
  def getNext: Iter[A]
}

def sumElements[A](f:A => Int)(xs:Iter[A]): Int = {
  xs.getValue match {
    case None => 0
    case Some(n) => f(n) + sumElements(f)(xs.getNext)
  }
}

class ListIter[A](val list: List[A]) extends Iter[A] {
  override def getValue: Option[A] = list.headOption
  override def getNext: Iter[A] = new ListIter[A](list.tail)
}

trait Dict[K,V] {
  def add(k: K, v: V): Dict[K,V]
  def find(k: K): Option[V]
}

class ListIterDict[K,V](eq: (K,K)=>Boolean, list: List[(K,V)]) extends ListIter[(K,V)](list) with Dict[K,V] {
  override def add(k: K, v: V): ListIterDict[K, V] = new ListIterDict(eq,(k,v)::list)
  override def find(k: K): Option[V] = {
    @tailrec
    def go(l: List[(K,V)]) : Option[V] = {
      l.headOption match {
        case None => None
        case Some(n) =>
          if(eq(k,n._1)) Some(n._2)
          else go(l.tail)
      }
    }
    go(list)
  }
}

def find3(d: Dict[Int,String]) = {
  d.find(3)
}

val d0: ListIterDict[Int,String] = new ListIterDict[Int,String]((x,y)=>(x==y),Nil)
val d: ListIterDict[Int,String] = d0.add(4,"four").add(3,"three")

find3(d)
sumElements[(Int,String)]((x:(Int,String))=>x._1)(d)