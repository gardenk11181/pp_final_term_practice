trait Ord[A] {
  def cmp(me: A, you: A): Int

  def ===(me: A, you: A): Boolean = cmp(me,you) == 0
  def <  (me: A, you: A): Boolean = cmp(me,you)<0
  def >  (me: A, you: A): Boolean = cmp(me,you)>0
  def <= (me: A, you: A): Boolean = cmp(me,you)<=0
  def >= (me: A, you: A): Boolean = cmp(me,you)>=0
}

def max3[A](a: A, b: A, c: A)(implicit ord: Ord[A]): A =
  if(ord.<=(a,b)) { if(ord.<=(b,c)) c else b}
  else {if(ord.<=(a,c)) c else a}
//
//implicit val intOrd: Ord[Int] = new Ord[Int] {
//  def cmp(me: Int, you: Int): Int = Integer.compare(me,you)
//}

implicit val revIntOrd: Ord[Int] = new Ord[Int] {
  def cmp(me: Int, you: Int): Int = Integer.compare(you,me)
}

max3(3,2,10)

// if just Bag[A] -> need to attach (implicit val ord: Ord[A])
class Bag[A: Ord] protected(val toList: List[A]) {
  def this() = this(Nil)

  def add(x: A): Bag[A] = {
    def go(elem: List[A]): List[A] =
      elem match {
        case Nil => x::Nil
        case hd::tl if(implicitly[Ord[A]].<(x,hd)) => x::elem
        case hd::tl if(implicitly[Ord[A]].===(x,hd)) => elem
        case hd::tl => hd::go(tl)
      }
    new Bag(go(toList))
  }
}

new Bag[Int]().add(3).add(2).add(3).add(10).toList

implicit def tupOrd[A: Ord,B: Ord]: Ord[(A,B)] = new Ord[(A,B)] {
  def cmp(me: (A,B), you: (A,B)): Int = {
    val c = implicitly[Ord[A]].cmp(me._1,you._1)
    if(c!=0) c
    else implicitly[Ord[B]].cmp(me._2,you._2)
  }
}

val b = new Bag[(Int,(Int,Int))]
b.add((3,(3,4))).add(3,(2,7)).add(4,(0,0)).toList