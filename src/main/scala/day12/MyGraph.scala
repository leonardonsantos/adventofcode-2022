package day12

import scala.collection.mutable
import math.Numeric.Implicits.infixNumericOps
import math.Ordering.Implicits.infixOrderingOps

class MyGraph[A,B](vertices: Set[A], adjList: Map[A, Set[(A, B)]]) (implicit num: Numeric[B]) {
  def getVertices: Set[A] = vertices
  def getAdjList: Map[A, Set[(A, B)]] = adjList

  def distanceFrom(source: A): (Map[A,B], Map[A,A]) = {
    val done = mutable.HashSet[A]()
    val dist = mutable.HashMap[A, B]()
    val minHeap = mutable.PriorityQueue[(A, B)]()(Ordering.by[(A, B), B](_._2).reverse)
    val prev = mutable.HashMap[A, A]()

    dist(source) = num.zero
    minHeap.enqueue((source, num.zero))
    done += source

    while (minHeap.nonEmpty) {
      val (v, _) = minHeap.dequeue()
      adjList.getOrElse(v,Set()).foreach{case (u,w) => {
        if (dist.contains(u)) {
          if (dist(v) + w < dist(u)) {
            dist(u) = dist(v) + w
            prev(u) = v
            minHeap.enqueue((u, dist(u)))
          }
        } else {
          dist(u) = dist(v) + w
          prev(u) = v
          minHeap.enqueue((u, dist(u)))
        }
      }}
      done += v
    }

    (dist.toMap, prev.toMap)
  }

  def shortestPath(source: A, destination: A): Option[(Seq[A], B)] = {
    val (dist, prev) = distanceFrom(source)
    if (dist.contains(destination))
      Some((buildPath(source, destination, prev.toMap), dist(destination)))
    else
      None
  }

  def buildPath(source: A, destination: A, prev: Map[A, A]): Seq[A] = {
    if (source == destination) {
      Seq(destination)
    } else if (prev.contains(destination)) {
      buildPath(source, prev(destination), prev) :+ destination
    } else {
      Seq.empty
    }
  }
}

/*
object main {
  def main(args: Array[String]): Unit = {
    val myGraph1 = new MyGraph[Int,Int](
      Set(1,2,3,4,5,6),
      Map(
        1 -> Set((2, 10), (5, 3)),
        2 -> Set((3, 2), (5, 4)),
        3 -> Set((4, 9)),
        4 -> Set((3, 9)),
        5 -> Set((2, 1), (3, 8), (4, 2)),
        6 -> Set((1, 2))
      )
    )

    assert(myGraph1.shortestPath(1, 2).contains((List(1, 5, 2), 4)))
    assert(myGraph1.shortestPath(1, 3).contains((List(1, 5, 2, 3), 6)))
    assert(myGraph1.shortestPath(1, 6).isEmpty)

    println("Done")
  }
}
*/