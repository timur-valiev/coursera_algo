package timur.algo.mincut

import util.Random

object Cutter extends App{
  Random.setSeed(System.currentTimeMillis())
  val inp1 = scala.io.Source.fromURL("http://spark-public.s3.amazonaws.com/algo1/programming_prob/kargerMinCut.txt").getLines().toIndexedSeq
  val inp = inp1  map (_.split("\t").toIndexedSeq map (_.toInt))

  //val inp1 = scala.io.Source.fromFile("test.txt").getLines().toIndexedSeq
  //val inp = inp1  map (_.split(" ").toIndexedSeq map (_.toInt))

  val gr  = (for {
    row <- inp
    t <- row.tail
  } yield Edge(row.head, t)).foldLeft(Graph())(_ + _)

  def cost(v1:Vertex, v2:Vertex):Int  = {
    val ans = (v1.merged+v1.name).foldLeft(0)((sum, v)=>sum + gr.get(v).neighbours.filter((v2.merged+v2.name).contains(_)).size)
    ans
  }

  def costOfReduced(g:Graph):Int = {
    if (g.vs.size==2){
      val c = g.vs.toIndexedSeq
      cost(c(0)._2,c(1)._2)
    } else
      costOfReduced(g.randomMerge)
  }


  def getmin(cur:Int):Int = {
    val min  = costOfReduced(gr)
    if (min<cur){
      println(min)
      Random.setSeed(System.currentTimeMillis())
    }
    getmin(Math.min(cur, min))
  }

  getmin(gr.vs.size-1)
}

case class Graph(vs: Map[Int, Vertex] = Map()) {
  def +(e: Edge) = {
    val s = get(e.s)
    val t = get(e.t)
    new Graph(vs + (s.name -> (s connect  e.t)) + (t.name -> t))
  }

  def get(name: Int) = {
    vs.getOrElse(name, Vertex(name))
  }

  def merge(e: Edge) = {
    val mm =vs + (e.s -> (get(e.s) merge get(e.t))) - e.t
    val m = for ((name, vertex) <- mm) yield {
      if (vertex.neighbours.contains(e.t) && vertex.name!=e.s)
        (name -> Vertex(vertex.name, vertex.neighbours - e.t + e.s, vertex.merged ))
      else
        (name->vertex)
    }
    //println(e)
    //println(vs)
    //println(mm)
    new Graph(m)
  }

  def getRandomVertex: Vertex = vs.toStream(Random.nextInt(vs.size))._2

  def getRandomEdge: Edge = getRandomVertex.getRandomEdge

  def randomMerge = merge(getRandomEdge)
}

case class Edge(s: Int, t: Int)

case class Vertex(name: Int, neighbours: Set[Int] = Set(), merged:Set[Int] = Set()) {
  def connect(v: Int) = Vertex(name, neighbours + v)

  def merge(v: Vertex) = Vertex(name, neighbours ++ v.neighbours - name, (merged ++ (v.merged + v.name)))

  def getRandomEdge: Edge = Edge(name, neighbours.toStream(Random.nextInt(neighbours.size)))
}

