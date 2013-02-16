def swap[T](arr:IndexedSeq[T], i:Int, j:Int):IndexedSeq[T] = {
  if (i==j || math.min(i,j)<0)
    arr
  else
    (arr.slice(0,i) :+ arr(j)) ++ arr.slice(i+1,j) ++ (arr(i) +: arr.slice(j+1, arr.size))
}

def cycle(seq: IndexedSeq[Int]): IndexedSeq[Int] = seq match {
  case IndexedSeq() => seq
  case _ => seq.tail :+ seq.head
}

def partition(ys:IndexedSeq[Int]):(IndexedSeq[Int], IndexedSeq[Int]) = {
  def update(pair: (IndexedSeq[Int], IndexedSeq[Int]), curr: Int) : (IndexedSeq[Int], IndexedSeq[Int]) = {
    val (l,r) = pair
    curr match {
      case x if x > ys.head => (l,r :+ curr)
      case x if x < ys.head => (l :+ curr, cycle(r))
      case _ => (l,r)
    }
  }
  ys.foldLeft((IndexedSeq[Int](),IndexedSeq[Int]()))(update)
}

def revCycle(seq: IndexedSeq[Int]): IndexedSeq[Int] = seq match {
  case IndexedSeq() => seq
  case _ => seq.last +: seq.slice(0, seq.size-1)
}

def quickSort(xs:IndexedSeq[Int]):Int  = {
  if(xs.size < 2)
    0
  else {
    val (l,r) = partition(xs)
    xs.size - 1 + quickSort(revCycle(l)) + quickSort(r)
  }
}

def quickSort2(xs:IndexedSeq[Int]):Int  = {
  if(xs.size < 2)
    0
  else {
    val ys = swap(xs,0, xs.size-1)
    val (l,r) = partition(ys)
    xs.size - 1 + quickSort2(revCycle(l)) + quickSort2(r)
  }
}

def quickSort3(xs:IndexedSeq[Int]):Int  = {
  if(xs.size < 2)
    0
  else {
    val x = (xs(0) :: xs((xs.size-1)/2) :: List(xs.last)).sorted
    val ys = swap(xs,0, xs.indexOf(x(1)))
    val (l,r) = partition(ys)
    xs.size - 1 + quickSort3(revCycle(l)) + quickSort3(r)
  }
}
quickSort(IndexedSeq(1, 11, 5, 15, 2, 12, 9, 99, 77, 0))
quickSort2(IndexedSeq(1, 11, 5, 15, 2, 12, 9, 99, 77, 0))
quickSort3(IndexedSeq(1, 11, 5, 15, 2, 12, 9, 99, 77, 0))
quickSort(IndexedSeq(2, 8, 9, 3, 7, 5, 10, 1, 6, 4))
quickSort2(IndexedSeq(2, 8, 9, 3, 7, 5, 10, 1, 6, 4))
quickSort3(IndexedSeq(2, 8, 9, 3, 7, 5, 10, 1, 6, 4))
quickSort( scala.io.Source.fromURL("http://spark-public.s3.amazonaws.com/algo1/programming_prob/QuickSort.txt").getLines().toIndexedSeq map (_.toInt))
quickSort2( scala.io.Source.fromURL("http://spark-public.s3.amazonaws.com/algo1/programming_prob/QuickSort.txt").getLines().toIndexedSeq map (_.toInt))
quickSort3( scala.io.Source.fromURL("http://spark-public.s3.amazonaws.com/algo1/programming_prob/QuickSort.txt").getLines().toIndexedSeq map (_.toInt))
 quickSort3 (Array(3,2,1,4,5))

