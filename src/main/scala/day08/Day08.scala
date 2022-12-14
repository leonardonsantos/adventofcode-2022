package day08

import org.apache.spark.sql.SparkSession
import scala.io.Source

object main {
  def parseMatrix(lines: Seq[String]): Array[Array[Int]] = {
    lines.map(_.map(_.asDigit).toArray).toArray
  }

  def isVisible(i:Int, j:Int, matrix: Array[Array[Int]]): Boolean = {
    val height = matrix(i)(j)
    val isVisibleLeft: Boolean = (0 until j)
      .foldLeft(true)((acc,x) => acc && (matrix(i)(x)<height))
    val isVisibleRight: Boolean = (j+1 until matrix(i).length)
      .foldLeft(true)((acc,x) => acc && (matrix(i)(x)<height))
    val isVisibleTop: Boolean = (0 until i)
    .foldLeft(true)((acc,x) => acc && (matrix(x)(j)<height))
    val isVisibleDown: Boolean = (i+1 until matrix.length)
      .foldLeft(true)((acc,x) => acc && (matrix(x)(j)<height))
    isVisibleLeft || isVisibleRight || isVisibleTop || isVisibleDown
  }

  def scenicScore(i:Int, j:Int, matrix: Array[Array[Int]]): Long = {
    val height = matrix(i)(j)
    val treesLeft = (j-1 to 0 by -1)
      .foldLeft((0,0)){ case ((acc,maxHeight),x) => {
        val thisHeight = matrix(i)(x)
        if (maxHeight>=height)
          (acc,maxHeight)
        else
          (acc+1,maxHeight.max(thisHeight))
      }}
      ._1
    val treesRight = (j+1 until matrix(i).length)
      .foldLeft((0,0)){ case ((acc,maxHeight),x) => {
        val thisHeight = matrix(i)(x)
        if (maxHeight>=height)
          (acc,maxHeight)
        else
          (acc+1,maxHeight.max(thisHeight))
      }}
      ._1
    val treesTop = (i-1 to 0 by -1)
      .foldLeft((0,0)){ case ((acc,maxHeight),x) => {
        val thisHeight = matrix(x)(j)
        if (maxHeight>=height)
          (acc,maxHeight)
        else
          (acc+1,maxHeight.max(thisHeight))
      }}
      ._1
    val treesDown = (i+1 until matrix.length)
      .foldLeft((0,0)){ case ((acc,maxHeight),x) => {
        val thisHeight = matrix(x)(j)
        if (maxHeight>=height)
          (acc,maxHeight)
        else
          (acc+1,maxHeight.max(thisHeight))
      }}
      ._1
    treesLeft * treesRight * treesTop * treesDown
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day08/input.txt"

    val spark = SparkSession
      .builder()
      .master("local[*]")
      .appName("Advent of Code 2022")
      .config("spark.kryo.registrationRequired","false")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")

    val lines = Source.fromFile(filename).getLines.toSeq
    val matrix = parseMatrix(lines)
    val matrixBC = spark.sparkContext.broadcast(matrix)

    val rddPositions = spark.sparkContext.parallelize(
      for {i<-0 until matrix.length; j<-0 until matrix(0).length} yield {(i,j)}
    )

    val result1 = rddPositions
      .filter{case (i,j) => isVisible(i,j, matrixBC.value)}
      .count
    println("Result1 = " + result1)

    val result2 = rddPositions
      .map{case (i,j) => scenicScore(i,j, matrixBC.value)}
      .reduce((a,b) => a.max(b))
    println("Result2 = " + result2)

  }
}
