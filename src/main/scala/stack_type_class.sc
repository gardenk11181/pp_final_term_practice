trait Stack[S,A] {
  def empty: S
  def get(s: S): (A,S)
  def put(s: S)(a: A): S
}

def testStack[S](implicit stk: Stack[S,Int]) = {
  val s0 = stk.empty
  val s1 = stk.put(s0)(3)
  val s2 = stk.put(s1)(-2)
  val s3 = stk.put(s2)(4)
  val (v1,s4) = stk.get(s3)
  val (v2,s5) = stk.get(s4)
  (v1,v2)
}

implicit def StackListInt: Stack[List[Int],Int] = new Stack[List[Int],Int] {
  override def empty: List[Int] = Nil
  override def get(s: List[Int]): (Int, List[Int]) = (s.head,s.tail)
  override def put(s: List[Int])(a: Int): List[Int] = a::s
}

testStack

def IntStackWithTopping[S](parent: Stack[S,Int], newPut: (S,Int) => S): Stack[S,Int] = new Stack[S,Int] {
  override def empty: S = parent.empty
  override def get(s: S): (Int, S) = parent.get(s)
  override def put(s: S)(a: Int): S = newPut(s,a)
}

def Doubling[S](parent: Stack[S,Int]): Stack[S,Int] =
  IntStackWithTopping(parent,(s,x)=>parent.put(s)(2*x))

def Incrementing[S](parent: Stack[S,Int]): Stack[S,Int] =
  IntStackWithTopping(parent,(s,x)=>parent.put(s)(x+1))

def Filtering[S](parent: Stack[S,Int]): Stack[S,Int] =
  IntStackWithTopping(parent,(s,x)=>if(x>=0) parent.put(s)(x) else s)

testStack(Filtering(Incrementing(Doubling(StackListInt))))

def SortedStackListInt: Stack[List[Int],Int] = new Stack[List[Int],Int] {
  override def empty: List[Int] = Nil
  override def get(s: List[Int]): (Int, List[Int]) = (s.head,s.tail)
  override def put(s: List[Int])(a: Int): List[Int] = {
    s match {
      case Nil => a::Nil
      case hd::tl =>
        if(hd<a) hd::put(tl)(a)
        else a::(hd::tl)
    }
  }
}

testStack(Filtering(Incrementing(Doubling(SortedStackListInt))))