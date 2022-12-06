package day06

import scala.io.Source
import scala.collection.mutable.Queue

object main {
  def isAllDiff(buffer: Queue[Char], diffSize: Int): Boolean = {
    // TODO: refactor
    buffer.toSet.size == diffSize
  }

  def findMarker(stream: String, diffSize: Int): Long = {
    val buffer = Queue[Char]()
    buffer.addAll(stream.substring(0,diffSize))
    stream
      .zipWithIndex
      .drop(diffSize)
      .foldLeft((' ',0)){case (acc,(c,i)) => {
        val result = if (isAllDiff(buffer, diffSize) && acc._2==0) (c,i) else acc
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
    println("Result1 = " + findMarker(stream, 4))

    println("Result2 = " + findMarker(stream, 14))

  }
}
