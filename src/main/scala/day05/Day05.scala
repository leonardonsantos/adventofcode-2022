package day05

import org.apache.spark.sql.SparkSession
import scala.io.Source
import scala.collection.mutable.Stack

object main {
  def parseInitialState(lines: Seq[String]): Array[Stack[Char]] = {
    val nStacks = 9 // TODO: parse this number
    val matrix = lines
      .takeWhile(!_.startsWith(" 1"))
      .zipWithIndex
      .map{case (line,i) => {
        (for(j <- 0 until nStacks) yield {
          line(j*4 + 1)
        }).toArray
      }}.toArray
    matrix
      .transpose
      .map(x => Stack().pushAll(x.filter(_!=' ').reverse))
  }

  def parseCommands(lines: Seq[String]): Seq[(Int,Int,Int)] = {
    val pattern = """move (\d+) from (\d+) to (\d+)""".r
    lines
      .filter(_.startsWith("move "))
      .map ( _ match {
          case pattern(a,b,c) => (a.toInt, b.toInt, c.toInt)
          case _ => (0, 0, 0)
      })
  }

  def printStacks(stacks: Array[Stack[Char]]): Unit = {
    stacks.foreach(println(_))
  }

  def topStacks(stacks: Array[Stack[Char]]): String =
    stacks.map(_.top).mkString

  def applyCommands(stacks: Array[Stack[Char]], commands: Seq[(Int,Int,Int)]): Unit = {
    commands.foreach{case (a,b,c) => {
      for(i<-0 until a) {
        val x = stacks(b-1).pop
        stacks(c-1).push(x)
      }
    }}
  }

  def applyCommands2(stacks: Array[Stack[Char]], commands: Seq[(Int,Int,Int)]): Unit = {
    commands.foreach{case (a,b,c) => {
      val popElements = (for(i<-0 until a) yield {
          stacks(b-1).pop
        }).reverse
      stacks(c-1).pushAll(popElements)
    }}
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day05/input.txt"
    // No spark 😢

    val lines = Source.fromFile(filename).getLines.toSeq
    val stacks = parseInitialState(lines)
    val commands = parseCommands(lines)

    applyCommands(stacks, commands)
    println("Result1 = " + topStacks(stacks))

    val stacks2 = parseInitialState(lines)
    applyCommands2(stacks2, commands)
    println("Result2 = " + topStacks(stacks2))
  }
}
