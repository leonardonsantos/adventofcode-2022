package day02

import org.apache.spark.sql.SparkSession
import scala.io.Source

object main {
  def parseLine(line: String): (String, String) = {
    val two = line.split(" ").take(2)
    (two.head, two.tail.head)
  }

  def calculatePoints(pair: (String, String)): Int = {
    pair match {
      case ("A","X") => 3 + 1
      case ("A","Y") => 6 + 2
      case ("A","Z") => 0 + 3
      case ("B","X") => 0 + 1
      case ("B","Y") => 3 + 2
      case ("B","Z") => 6 + 3
      case ("C","X") => 6 + 1
      case ("C","Y") => 0 + 2
      case ("C","Z") => 3 + 3
      case _ => 0
    }
  }

  def calculatePoints2(pair: (String, String)): Int = {
    pair match {
      case ("A","X") => 0 + 3
      case ("A","Y") => 3 + 1
      case ("A","Z") => 6 + 2
      case ("B","X") => 0 + 1
      case ("B","Y") => 3 + 2
      case ("B","Z") => 6 + 3
      case ("C","X") => 0 + 2
      case ("C","Y") => 3 + 3
      case ("C","Z") => 6 + 1
      case _ => 0
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day02/input.txt"

    val spark = SparkSession
      .builder()
      .master("local[*]")
      .appName("Advent of Code 2022")
      .config("spark.kryo.registrationRequired","false")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")

    val textRdd = spark.sparkContext.textFile(filename)
    val result1: Int = textRdd
      .map(parseLine)
      .map(calculatePoints)
      .reduce(_+_)

    println("Result1 = " + result1)

    val result2: Int = textRdd
      .map(parseLine)
      .map(calculatePoints2)
      .reduce(_+_)

    println("Result2 = " + result2)

  }
}
