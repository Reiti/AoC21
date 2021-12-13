import util.Util

object Day13 {
  def main(args: Array[String]): Unit = {
    val input = Util.loadDayLines(13)
    val split = input.splitAt(input.indexWhere(_ == ""))

    val paper = split._1.map(line => {
      val s = line.split(",")
      (s(0).toInt, s(1).toInt) -> '#'
    }).toMap

    val folds = split._2.tail.map(line => {
      val s = line.split(" ")(2).split("=")
      (s(0), s(1).toInt)
    })

    //Part 1
    println(foldOnce(folds.head, paper).values.count(_ == '#'))

    //Part 2
    printMap(folds.foldLeft(paper)((acc, v) => foldOnce(v, acc)))
  }

  def foldOnce(fold: (String, Int), paper: Map[(Int, Int), Char]): Map[(Int, Int), Char] = paper.keys.map(k => {
    fold._1 match {
      case "y" => (k._1, fold._2 - Math.abs(k._2 - fold._2)) -> paper(k)
      case "x" => (fold._2 - Math.abs(k._1 - fold._2), k._2) -> paper(k)
    }
  }).toMap

  def printMap(map: Map[(Int, Int), Char]): Unit = {
    val maxX = map.keys.maxBy(_._1)._1
    val maxY = map.keys.maxBy(_._2)._2

    for(j <- 0 to maxY) {
      for(i <- 0 to maxX) {
        print(map.getOrElse((i, j), '.'))
      }
      println()
    }
  }
}
