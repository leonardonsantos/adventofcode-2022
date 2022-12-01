package day01

import org.apache.spark.sql.SparkSession
import scala.io.Source

object main {
  def parseInput(input: Iterator[String]): List[List[Int]] = {
    input.foldLeft (List[List[Int]]()) {
      (acc, x) => {
        if (x == "") {
          List[Int]() :: acc
        } else {
          if (acc.isEmpty)
            List(List(x.toInt))
          else
            (x.toInt :: acc.head) :: acc.tail
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day01/input.txt"
    val groups: List[List[Int]] = parseInput(Source.fromFile(filename).getLines)

    /*
    val spark = SparkSession
      .builder()
      .master("local[*]")
      .appName("Advent of Code 2022")
      .getOrCreate()

    val rddGroups = spark.sparkContext.parallelize(groups)
    println(rddGroups.collect())
    */

    val result1 = groups.map(_.sum).max
    println("Result1 = " + result1)

    val result2 = groups.map(_.sum).sortBy(-_).take(3).sum
    println("Result2 = " + result2)

  }
}
