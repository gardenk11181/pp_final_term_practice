/* Structural SubType ( Implicit )

Nothing :< T :< S :< Any
T :< T', S :< S' => (T'=>S) :< (T=>S')

*/

// syntactic sugar
{
  class MyTree[A](val value: A, val left: Option[MyTree[A]], val right: Option[MyTree[A]])

  type YourTree[A] = Option[MyTree[A]]

  val t0: YourTree[Int] = None;
  val t1: YourTree[Int] = Some(new MyTree[Int](3, None, None))
}

// Overriding
{
  class foo_type(x: Int, y: Int) {
    val a: Int = x
    val b: Int = a + y

    def f(z: Int): Int = b + y + z
  }
  class gee_type(x: Int) extends foo_type(x + 1, x + 2) {
    override def f(z: Int): Int = b + z

    //  override def f(z: Int): Int = super.f(z)*2
    val c: Int = f(x) + b
  }
  (new gee_type(30)).c
}

// case class : for skip type checking etc

{
  sealed abstract class MyList[A] // sealed: inherit in only this file
  case class MyNil[A]() extends MyList[A]
  case class MyCons[A](hd: A, tl: MyList[A]) extends MyList[A]

  val t: MyList[Int] = MyCons(3, MyNil())

  def length(x: MyList[Int]): Int = {
    x match {
      case MyNil() => 0
      case MyCons(hd, tl) => 1 + length(tl)
    }
  }

  sealed abstract class MyTree[A]
  case class Empty[A]() extends MyTree[A]
  case class Node[A](v: A, lt: MyTree[A], rt: MyTree[A]) extends MyTree[A]

  val s: MyTree[Int] = Node(1,Node(4,Empty(),Empty()),Empty())
}

// Interface

{
  abstract class Iter[A] {
    def getValue: Option[A]
    def getNext: Iter[A]
  }

  def sumElements[A](f: A=>Int)(xs: Iter[A]): Int =
    xs.getValue match {
      case None => 0
      case Some(n) => f(n)+sumElements(f)(xs.getNext)
    }
  def sumElementsId(xs: Iter[Int]) = sumElements((x:Int)=>x)(xs)

  sealed abstract class MyList[A] extends Iter[A]
  case class MyNil[A]() extends MyList[A] {
    def getNext: Iter[A] = throw new Exception()
    def getValue: Option[A] = None
  }
  case class MyCons[A](hd: A, tl: MyList[A]) extends MyList[A] {
    def getNext: Iter[A] = tl
    def getValue: Option[A] = Some(hd)
  }
  val t = MyCons(3,MyCons(6,MyCons(7,MyNil())))

  sumElementsId(t)

  class IntCounter(n: Int) extends Iter[Int] {
    def getValue: Option[Int] = if(n<0) None else Some(n)
    def getNext: Iter[Int] = new IntCounter(n-1)
  }

  sumElementsId(new IntCounter(100))
}

// Iter & Iterable (Tree: Iterable use List Iter)
{
  abstract class Iter[A] {
    def getValue: Option[A]
    def getNext: Iter[A]
  }
  abstract class Iterable[A] {
    def iter: Iter[A]
  }

  def sumElements[A](f: A=>Int)(xs: Iter[A]): Int =
    xs.getValue match {
      case None => 0
      case Some(n) => f(n)+sumElements(f)(xs.getNext)
    }
  def sumElementsGen[A](f: A=>Int)(xs: Iterable[A]): Int = {
    sumElements(f)(xs.iter)
  }

  sealed abstract class MyList[A] extends Iter[A] {
    def append(lst: MyList[A]): MyList[A]
  }
  case class MyNil[A]() extends MyList[A] {
    def getNext: Iter[A] = throw new Exception()
    def getValue: Option[A] = None
    def append(lst: MyList[A]): MyList[A] = lst
  }
  case class MyCons[A](hd: A, tl: MyList[A]) extends MyList[A] {
    def getNext: Iter[A] = tl
    def getValue: Option[A] = Some(hd)
    def append(lst: MyList[A]): MyList[A] = MyCons(hd,tl.append(lst))
  }

  sealed abstract class MyTree[A] extends Iterable[A] {
    def iter: MyList[A]
  }
  case class Empty[A]() extends MyTree[A]{
    def iter: MyList[A] = MyNil()
  }
  case class Node[A](v: A, lt: MyTree[A], rt: MyTree[A]) extends MyTree[A]{
    override val iter: MyList[A] = MyCons(v,lt.iter.append(rt.iter))
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

  sumElementsGen((x:Int)=>x)(generateTree(100))
}