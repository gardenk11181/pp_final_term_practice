// Lazy List

abstract class Iter[A] {
  def getValue: Option[A]
  def getNext: Iter[A]
}
abstract class Iterable[A] {
  def iter: Iter[A]
}
def time[R](block: => R):R = {
  val t0 = java.lang.System.nanoTime()
  val result = block
  val t1 = java.lang.System.nanoTime()
  println("Elapsed time: "+ ((t1-t0)/1000000) + "ms"); result
}
def sumN[A](f: A=> Int)(n: Int, xs: Iterable[A]): Int = {
  def sumIter(res : Int, n: Int, xs: Iter[A]): Int =
    if (n <=0) res
    else xs.getValue match {
      case None => res
      case Some(v) => sumIter(f(v) + res, n-1, xs.getNext)
    }
  sumIter(0,n,xs.iter)
}

abstract class LazyList[A] extends Iter[A] {
  def head: Option[A]
  def tail: LazyList[A]
  def append(lst: LazyList[A]): LazyList[A]
  def getValue: Option[A] = head
  def getNext: LazyList[A] = tail
}
case class LNil[A]() extends LazyList[A] {
  val head = None
  val tail = this
  def append(lst: LazyList[A]): LazyList[A] = lst
}
// case class => call by value
class LCons[A](hd: => A, tl: => LazyList[A]) extends LazyList[A] {
  lazy val head = Some(hd)
  lazy val tail = tl
  def append(lst: LazyList[A]) = LCons(hd,tl.append(lst))
}
object LCons{ def apply[A](hd: => A, tl: => LazyList[A]) = new LCons(hd,tl)}

sealed abstract class MyTree[A] extends Iterable[A] {
  def iter: LazyList[A]
}
case class Empty[A]() extends MyTree[A] {
  val iter = LNil()
}
case class Node[A](value: A, lt: MyTree[A], rt: MyTree[A]) extends MyTree[A] {
  lazy val iter = LCons(value,lt.iter.append(rt.iter))
}

def generateTree(n: Int) : MyTree[Int] = {
  def gen(lo: Int, hi: Int): MyTree[Int] = {
    if(lo>hi) Empty()
    else {
      val mid = (lo+hi)/2
      Node(mid,gen(lo,mid-1),gen(mid+1,hi))
    }
  }
  gen(1,n)
}

val t: MyTree[Int] = generateTree(200000)
time(sumN((x:Int)=>x)(100,t))
time(sumN((x:Int)=>x)(10000,t))
