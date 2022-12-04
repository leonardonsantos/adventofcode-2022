package day01

import org.apache.spark.sql.SparkSession

object main {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession
      .builder()
      .master("local[*]")
      .appName("Advent of Code 2022")
      .config("spark.kryo.registrationRequired","false")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")

    val filename = "src/main/scala/day01/input.txt"
    val textRdd = spark.sparkContext.textFile(filename)
    val withIndex = textRdd.zipWithIndex
    val groupIndexes: Seq[Long] = withIndex
      .filter{case (line, _) => line == ""}
      .map(_._2)
      .collect.toSeq
    val groupIndexesBC = spark.sparkContext.broadcast(groupIndexes)
    val rddGroups = withIndex
      .filter{case (line, _) => line != ""}
      .map{case (line,i) => {
        val group: Long = groupIndexesBC.value
          .foldLeft(0.toLong)((acc,j) => if (i>acc && i<j) acc else j)
        (group, line.toInt)
      }}
      .groupByKey
      .values

    val resultRdd1 = rddGroups.map(_.sum).max
    println("Result1 = " + resultRdd1)

    val resultRdd2 = rddGroups.map(_.sum).sortBy(-_).take(3).sum
    println("Result2 = " + resultRdd2)
  }
}
