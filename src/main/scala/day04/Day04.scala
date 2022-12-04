package day04

import org.apache.spark.sql.SparkSession
import scala.io.Source

object main {
  def parseLine(line: String): ((Int,Int),(Int,Int)) = {
    val elves = line.split(",")
    val as = elves(0).split("-")
    val bs = elves(1).split("-")
    (
      (as(0).toInt, as(1).toInt),
      (bs(0).toInt, bs(1).toInt)
    )
  }

  def isSubSection(tuple: ((Int,Int),(Int,Int))): Boolean = {
    val ((a,b),(c,d)) = tuple
    ((a<=c && b>=d) || (c<=a && d>=b))
  }

  def isOverlap(tuple: ((Int,Int),(Int,Int))): Boolean = {
    val ((a,b),(c,d)) = tuple
    (isSubSection(tuple) || (b>=c && a<c) || (a<=d && b>d) )
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day04/input.txt"

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
      .map(t => if (isSubSection(t)) 1 else 0)
      .reduce(_+_)

    println("Result1 = " + result1)

    val result2 = textRdd
      .map(parseLine)
      .map(t => if (isOverlap(t)) 1 else 0)
      .reduce(_+_)

    println("Result2 = " + result2)

  }
}
