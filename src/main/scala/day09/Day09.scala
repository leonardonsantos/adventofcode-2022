package day09

import scala.io.Source

object main {
  def parseInput(lines: Seq[String]): Seq[(String,Int)] = {
    lines.map(line => {
      val parts = line.split(" ")
      (parts(0), parts(1).toInt)
    })
  }

  def solution1(commands: Seq[(String,Int)]): Set[(Int,Int)] = {
    case class Accumulator(posH: (Int,Int), posT: (Int,Int), visited: Set[(Int,Int)])
    val initialAcc = Accumulator((0,0), (0,0), Set((0,0)))
    val finalAcc = commands.foldLeft(initialAcc)((acc,command) => {
      val (c,d) = command
      val (newH, newT, newVisited) = (1 to d).foldLeft((acc.posH,acc.posT,acc.visited))((acc2,i) => {
        val (posH, posT, visited) = acc2
        val newH = c match {
          case "R" => (posH._1,posH._2+1)
          case "L" => (posH._1,posH._2-1)
          case "U" => (posH._1-1,posH._2)
          case "D" => (posH._1+1,posH._2)
          case _ => posH
        }
        val newT = {
          val moveTa =
            if (newH._1 - posT._1 > 1) 1
            else if (newH._1 - posT._1 < -1) -1
            else 0
          val moveTb =
            if (newH._2 - posT._2 > 1) 1
            else if (newH._2 - posT._2 < -1) -1
            else 0
          if (posT._1 == newH._1) { // same line
            (posT._1, posT._2 + moveTb)
          } else if (posT._2 == newH._2) { // same column
            (posT._1 + moveTa, posT._2)
          } else if (moveTa!=0 || moveTb!=0) { // jump in diagonal
            val newMoveTa = if (newH._1 < posT._1) -1 else 1
            val newMoveTb = if (newH._2 < posT._2) -1 else 1
            (posT._1 + newMoveTa, posT._2 + newMoveTb)
          } else {
            posT
          }
        }

        val newVisited = visited + newT
        (newH, newT, newVisited)
      })

      Accumulator(newH, newT, newVisited)
    })

    finalAcc.visited
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day09/input.txt"
    // No spark 😢

    val lines = Source.fromFile(filename).getLines.toSeq
    val commands = parseInput(lines)

    val result1 = solution1(commands)
    println("Result1 = " + result1.size)
  }
}
