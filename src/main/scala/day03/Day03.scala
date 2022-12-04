package day03

import org.apache.spark.sql.SparkSession
import scala.io.Source

object main {
  def parseLine(line: String): (Seq[Int], Seq[Int]) = {
    val priorities: Seq[Int] = line.map(c => {
      if (c >= 'a')
        c.toInt - 'a'.toInt + 1
      else
        c.toInt - 'A'.toInt + 27
    })
    priorities.splitAt(priorities.length / 2)
  }

  def parseLine2(line: String): Seq[Int] = {
    val priorities: Seq[Int] = line.map(c => {
      if (c >= 'a')
        c.toInt - 'a'.toInt + 1
      else
        c.toInt - 'A'.toInt + 27
    })
    priorities
  }

  def solution1(tuple: (Seq[Int], Seq[Int])): Int = {
    val (a,b) = tuple
    a.toSet.intersect(b.toSet).head
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day03/input.txt"

    val spark = SparkSession
      .builder()
      .master("local[*]")
      .appName("Advent of Code 2022")
      .config("spark.kryo.registrationRequired","false")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")

    val textRdd = spark.sparkContext.textFile(filename)
    val result1 = textRdd
      .map(parseLine)
      .map(solution1)
      .reduce(_+_)

    println("Result1 = " + result1)

    val result2 = textRdd
      .zipWithIndex
      .map{case (row, i) => (i / 3, parseLine2(row))}
      .mapValues(_.toSet)
      .reduceByKey(_.intersect(_))
      .values
      .map(_.head)
      .reduce(_+_)

    println("Result2 = " + result2)

  }
}
