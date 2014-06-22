def scalarProduct(xs: List[Double], ys: List[Double]) : Double = {
  val f = for {
    i <- 0 until xs.length
  } yield xs(i)*ys(i)
  f.sum

}

def wordOccurrences(w: String): List[(Char, Int)] = {
  w.toList.groupBy(x => x.toLower).map { x => (x._1, x._2.length)}.toList.sortWith(_._1 < _._1)
}

def maxSum(l:List[Int], max: Int, cur: Int): Int = {
  l match {
    case Nil => max
    case x :: xs =>
     val curr:Int = cur match {
       case y if y <=0 => x
       case z => x + cur
     }
     val ma:Int = curr match {
       case y if y > max => y
       case z => z
     }
     maxSum(xs, ma, curr)
  }
}

def flatMap[A,B](x: List[A], f:A => List[B]) : List[B] = {
  x match {
    case Nil => Nil
    case head :: rem  => f(head) ::: flatMap(rem, f)
  }
}

flatMap[Int, Int](List(1,2,3,4), x => List(x, x + 1))

maxSum(List(-4, -1, 3, -1, 1, -1, 2, 6), -1000, 0)
wordOccurrences("I lovie youuuu")
scalarProduct(List(2,3,4), List(3,4,5))