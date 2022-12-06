package day06

import scala.io.Source
import scala.collection.mutable.Queue

object main {
  def isAllDiff(buffer: Queue[Char]): Boolean = {
    // TODO: refactor
    buffer.toSet.size == 4
  }

  def findMarker(stream: String): Long = {
    val buffer = Queue[Char]()
    buffer.addAll(stream.substring(0,4))
    stream
      .zipWithIndex
      .drop(4)
      .foldLeft((' ',0)){case (acc,(c,i)) => {
        val result = if (isAllDiff(buffer) && acc._2==0) (c,i) else acc
        buffer.dequeue()
        buffer.enqueue(c)
        result
      }}
      ._2
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day06/input.txt"
    // No spark 😢

    val stream = scala.io.Source.fromFile(filename).mkString
    println("Result1 = " + findMarker(stream))

  }
}
