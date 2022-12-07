package day07

import scala.io.Source

object main {
  case class File(name: String, size: Long)
  case class Directory(
                        name: String,
                        var files: List[File],
                        var subDirectories: List[Directory],
                        parent: Option[Directory]
                      )

  case class Accumulator(var value: Long)

  def parseInput(lines: Seq[String]): Directory = {
    val rootDir = Directory("/",List(),List(),None)
    var currentDir = rootDir
    lines.drop(1).foreach(line => {
      val commandParts = line.split(" ")
      if (line.startsWith("$")) { // command
        if (commandParts(1) == "cd") {
          val subDir = commandParts(2)
          if (subDir == "/") {
            // ignore
          } else if (subDir == "..") {
            // go dir up
            currentDir = currentDir.parent.get
          } else {
            // change currentDir
            currentDir.subDirectories.foreach(d => {
              if (d.name == subDir)
                currentDir = d
            })
          }
        }
        // ignore "ls"
      } else if (line.startsWith("dir")) {
        // add subdir to currentDir
        currentDir.subDirectories = Directory(commandParts(1),List(),List(),Some(currentDir)) :: currentDir.subDirectories
      } else {
        // add file to currentDir
        currentDir.files = File(commandParts(1),commandParts(0).toLong) :: currentDir.files
      }
    })
    rootDir
  }

  def solution1(dir: Directory, accTotal: Accumulator): Long = {
    val filesSize = dir.files.foldLeft(0.toLong)((acc,x) => acc + x.size)
    val subDirsSize = dir.subDirectories.foldLeft(0.toLong)((acc,sd) => {
      acc + solution1(sd,accTotal)
    })

    val dirSize = filesSize + subDirsSize
    if (dirSize<=100000) {
      accTotal.value = accTotal.value + dirSize
    }

    dirSize
  }

  def solution1(dir: Directory): Long = {
    val acc = Accumulator(0)
    solution1(dir,acc)
    acc.value
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day07/input.txt"
    // No spark 😢

    val lines = Source.fromFile(filename).getLines.toSeq
    val dir = parseInput(lines)

    val result1 = solution1(dir)
    println("Result1 = " + result1)
  }
}
