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

  def solution1(commands: Seq[(String,Int)]): Int = {
    val registerValues = computeRegisterValues(commands)
    Seq(20,60,100,140,180,220)
      .map(signalStrength(registerValues,_))
      .sum
  }

  def solution2(commands: Seq[(String,Int)]): String = {
    val registerValues = computeRegisterValues(commands)

    registerValues
      .foldLeft((0,"")){case ((i,crt),x) => {
        val newI = if (i==39) 0 else i+1
        val newCrt = if (i>=x-1 && i<=x+1) crt+"#" else crt+"."
        val newCrt2 = if (newI==0) newCrt+"\n" else newCrt
        (newI, newCrt2)
      }}
      ._2
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day10/input.txt"
    // No spark 😢

    val lines = Source.fromFile(filename).getLines.toSeq
    val commands = parseInput(lines)

    val result1 = solution1(commands)
    println("Result1 = " + result1)

    val result2 = solution2(commands)
    println("Result2 = \n" + result2)

  }
}
