abstract class Iter[A] {
  def getValue : Option[A]
  def getNext: Iter[A]
}

class ListIter[A](val list: List[A]) extends Iter[A] {
  override def getValue: Option[A] = list.headOption
  override def getNext: Iter[A] = new ListIter[A](list.tail)
}

trait MRIter[A] extends Iter[A] {
  def mapReduce[B,C](combine: (B,C)=>C, ival: C, f: A=>B): C = {
    def go(c: Iter[A]): C = c.getValue match {
      case None => ival
      case Some(v) => combine(f(v),go(c.getNext))
    }
    go(this)
  }
}

class MRListIter[A](list: List[A]) extends ListIter[A](list) with MRIter[A]

val mr = new MRListIter[Int](List(3,4,5))

// syntactic sugar
val mr2 = new ListIter[Int](List(3,4,5)) with MRIter[Int]

mr.mapReduce[Int,Int]((b,c)=>b+c,0,(a)=>a*a)
mr2.mapReduce[Int,Int]((b,c)=>b+c,0,a=>a*a)