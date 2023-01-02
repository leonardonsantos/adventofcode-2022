package day12

import scala.io.Source
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.edge.Implicits._

object main {
  def testGraph() = {
    val g = Graph(1 ~ 2 % 4, 2 ~ 3 % 2, 1 ~> 3 % 5, 1 ~ 5  % 3,
                  3 ~ 5 % 2, 3 ~ 4 % 1, 4 ~> 4 % 1, 4 ~> 5 % 0)
    def n(outer: Int): g.NodeT = g.get(outer)  // look up 'outer' that is known to be contained
    val spO = n(3).shortestPathTo(n(1)) // Path(3, 3 ~ 4 % 1, 4, 4 ~> 5 % 0, 5, 1 ~ 5 % 3, 1)
    val sp = spO.get                   // here we know spO is defined
    println(sp.nodes)                  // List[g.NodeT] = Nodes(3, 4, 5, 1)
  }

  def parseInput(lines: Seq[String]): (Map[(Int, Int), Char], Graph[(Int, Int), DiEdge]) = {
    val matrix = lines
      .zipWithIndex
      .flatMap{case (line,i)=> {
        line
          .zipWithIndex
          .map{case (c,j) => ((i,j),c)}
      }}
      .toMap
    val matrix2 = matrix.map{case (k,v) => if (v=='E') (k,'z') else (k,v)}
    val nodes = matrix2.toSeq
      .flatMap{ case ((i,j),c) => {
        val left =
          if (matrix2.contains((i,j-1)))
            if (matrix2((i,j)).toInt+1 >= matrix2((i,j-1)).toInt || matrix2((i,j))=='S')
              Some((i,j) ~> (i,j-1))
            else
              None
          else
            None
        val right =
          if (matrix2.contains((i,j+1)))
            if (matrix2((i,j)).toInt+1 >= matrix2((i,j+1)).toInt || matrix2((i,j))=='S')
              Some((i,j) ~> (i,j+1))
            else
              None
          else
            None
        val up =
          if (matrix2.contains((i-1,j)))
            if (matrix2((i,j)).toInt+1 >= matrix2((i-1,j)).toInt || matrix2((i,j))=='S')
              Some((i,j) ~> (i-1,j))
            else
              None
          else
            None
        val down =
          if (matrix2.contains((i+1,j)))
            if (matrix2((i,j)).toInt+1 >= matrix2((i+1,j)).toInt || matrix2((i,j))=='S')
              Some((i,j) ~> (i+1,j))
            else
              None
          else
            None
        List(left, right, up, down).flatten
      }}
    val graph = nodes.foldLeft(Graph[(Int,Int), DiEdge]())(_+_)
    (matrix, graph)
  }

  def parseInputB(lines: Seq[String]): (Map[(Int, Int), Char], MyGraph[(Int,Int),Int]) = {
    val matrix = lines
      .zipWithIndex
      .flatMap{case (line,i)=> {
        line
          .zipWithIndex
          .map{case (c,j) => ((i,j),c)}
      }}
      .toMap
    val matrix2 = matrix.map{case (k,v) => if (v=='E') (k,'z') else (k,v)}
    val nodes = matrix2.toSeq
      .flatMap{ case ((i,j),c) => {
        val left =
          if (matrix2.contains((i,j-1)))
            if (matrix2((i,j)).toInt+1 >= matrix2((i,j-1)).toInt || matrix2((i,j))=='S')
              Some(((i,j), (i,j-1)))
            else
              None
          else
            None
        val right =
          if (matrix2.contains((i,j+1)))
            if (matrix2((i,j)).toInt+1 >= matrix2((i,j+1)).toInt || matrix2((i,j))=='S')
              Some(((i,j), (i,j+1)))
            else
              None
          else
            None
        val up =
          if (matrix2.contains((i-1,j)))
            if (matrix2((i,j)).toInt+1 >= matrix2((i-1,j)).toInt || matrix2((i,j))=='S')
              Some(((i,j), (i-1,j)))
            else
              None
          else
            None
        val down =
          if (matrix2.contains((i+1,j)))
            if (matrix2((i,j)).toInt+1 >= matrix2((i+1,j)).toInt || matrix2((i,j))=='S')
              Some(((i,j), (i+1,j)))
            else
              None
          else
            None
        List(left, right, up, down).flatten
      }}

    val vertices = nodes.foldLeft(Map[(Int,Int),Set[((Int,Int),Int)]]()){
      case (acc, (a,b)) => {
        val x = acc.getOrElse(a, Set[((Int,Int),Int)]())
        acc ++ Map(a -> (x.incl((b,1))))
      }

    }
    val graph = new MyGraph[(Int,Int),Int](vertices.keySet,vertices)

    (matrix, graph)
  }

  def findShortestPath(matrix: Map[(Int, Int), Char], graph: Graph[(Int, Int), DiEdge]): Option[graph.Path] = {
    def findValue(value: Char): (Int, Int) = matrix.find{case (k,v) => v == value}.get._1
    val posS = findValue('S')
    val posE = findValue('E')
    println(posS,posE)
    val path = graph.get(posS).shortestPathTo(graph.get(posE))
    path
  }

  def findShortestPathB(matrix: Map[(Int, Int), Char], graph: MyGraph[(Int, Int), Int]): Seq[(Int,Int)] = {
    def findValue(value: Char): (Int, Int) = matrix.find{case (k,v) => v == value}.get._1
    val posS = findValue('S')
    val posE = findValue('E')
    println(posS,posE)
    val path = graph.shortestPath(posS, posE)
    path.get._1
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day12/input.txt"
    // No spark 😢

    //testGraph()

    val lines = Source.fromFile(filename).getLines.toSeq
    val (matrix,graph) = parseInput(lines)

    /*
    val result1 = findShortestPath(matrix,graph)
    println("Result1 = " + result1.get.nodes)
    println("Result1 = " + (result1.get.nodes.size - 1))
     */

    val (matrixB,graphB) = parseInputB(lines)
    val result1B = findShortestPathB(matrix,graphB)
    println("Result1 = " + result1B)
    println("Result1 = " + (result1B.size - 1))

  }
}
