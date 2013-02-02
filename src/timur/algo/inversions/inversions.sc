import annotation.tailrec

type Info = (List[Int], BigInt)

def inversions(xs: List[Int]): Info = {
  def merge(xf: Info, yf: Info): Info = {
    @tailrec
    def mergeLoop(xxs: List[Int], yss: List[Int], takenFromSecond: Int, listAccu: List[Int], accu: BigInt): Info = {
      //println(xxs, yss, takenFromSecond, listAccu, accu)
      (xxs, yss) match {
        case (Nil, _) => (listAccu.reverse ::: yss, accu)
        case (_, Nil) => (listAccu.reverse ::: xxs, accu)
        case (x, y) => {
          if (x.head < y.head) {
            mergeLoop(x.tail, y, takenFromSecond, x.head :: listAccu, accu)
          } else {
            mergeLoop(x, y.tail, takenFromSecond + 1, y.head :: listAccu, accu + x.length)
          }
        }
      }
    }

    mergeLoop(xf._1, yf._1, 0, List(), xf._2 + yf._2)
  }

  if (xs.length < 2)
    (xs, 0)
  else {
    merge(inversions(xs.take(xs.length / 2)), inversions(xs.drop(xs.length / 2)))
  }
}
inversions(List(6,5,4,  2,3 , 1))


val txt = scala.io.Source.fromURL("http://spark-public.s3.amazonaws.com/algo1/programming_prob/IntegerArray.txt")
inversions( txt.getLines().toList map (_.toInt))._2


