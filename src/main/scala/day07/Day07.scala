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

  def solution2(dir: Directory, accTotal: Accumulator, neededSpace: Long): Long = {
    val filesSize = dir.files.foldLeft(0.toLong)((acc,x) => acc + x.size)
    val subDirsSize = dir.subDirectories.foldLeft(0.toLong)((acc,sd) => {
      acc + solution2(sd,accTotal, neededSpace)
    })

    val dirSize = filesSize + subDirsSize

    if (dirSize>=neededSpace && dirSize<accTotal.value) {
      accTotal.value = dirSize
    }

    dirSize
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/main/scala/day07/input.txt"
    // No spark 😢

    val lines = Source.fromFile(filename).getLines.toSeq
    val dir = parseInput(lines)

    val acc1 = Accumulator(0)
    val sizeDir = solution1(dir, acc1)
    val result1 = acc1.value
    println("Result1 = " + result1)

    val freeSpace = 70000000 - sizeDir
    val neededSpace = 30000000 - freeSpace
    val acc2 = Accumulator(Long.MaxValue)
    solution2(dir, acc2, neededSpace)
    val result2 = acc2.value
    println("Result2 = " + result2)
  }
}
