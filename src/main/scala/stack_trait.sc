trait Stack[A] {
  def get(): (A,Stack[A])
  def put(x: A): Stack[A]
}

class BasicIntStack protected(xs: List[Int]) extends Stack[Int] {
  override val toString = "Stack:" + xs.toString()
  def this() = this(Nil)
  def MStack(xs: List[Int]): Stack[Int] = new BasicIntStack(xs)
  def get(): (Int,Stack[Int]) = (xs.head,MStack(xs.tail))
  def put(x: Int): Stack[Int] = MStack(x::xs)
}

val s0 = new BasicIntStack
val s1 = s0.put(3)
val s2 = s1.put(-2)
val s3 = s2.put(4)
val (v1,s4) = s3.get()
val (v2,s5) = s4.get()

trait Doubling extends Stack[Int] {
  abstract override def put(x: Int): Stack[Int] = super.put(2*x)
}

class DIntStack protected(xs: List[Int]) extends BasicIntStack(xs) with Doubling {
  def this() = this(Nil)
  override def MStack(xs: List[Int]): Stack[Int] = new DIntStack(xs)
}

val t0 = new DIntStack
val t1 = t0.put(3)
val t2 = t1.put(-2)
val t3 = t2.put(4)
val (v1,t4) = t3.get()
val (v2,t5) = t4.get()