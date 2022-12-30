package day10

import scala.io.Source

object main {
  def parseInput(lines: Seq[String]): Seq[(String,Int)] = {
    lines.map(line => {
      val components = line.split(" ")
      val command = components(0)
      if (command == "noop")
        (command,0)
      else
        (command,components(1).toInt)
    })
  }

  def computeRegisterValues(commands: Seq[(String, Int)]): Seq[Int] = {
    commands
      .flatMap{ case (command,value) => if (command == "noop") Seq(0) else Seq(0,value)}
      .scanLeft(1)(_+_)
  }

  def signalStrength(registerValues: Seq[Int], cycle: Int): Int = {
    cycle * registerValues(cycle-1)
  }

  def solution1(commands: Seq[(String,Int)]) = {
    val registerValues = computeRegisterValues(commands)
    Seq(20,60,100,140,180,220)
      .map(signalStrength(registerValues,_))
      .sum
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day10/input.txt"
    // No spark 😢

    val lines = Source.fromFile(filename).getLines.toSeq
    val commands = parseInput(lines)

    val result1 = solution1(commands)
    println("Result1 = " + result1)
  }
}
