import util.Util

object Day13 {
  def main(args: Array[String]): Unit = {
    val input = Util.loadDayLines(13)
    val split = input.splitAt(input.indexWhere(_ == ""))

    val paper = buildMap(split._1)
    val folds = split._2.tail.map(_.split(" ")(2))

    //Part 1
    println(foldOnce(folds.head, paper).values.count(_ == '#'))

    //Part 2
    printMap(folds.foldLeft(paper)((acc, v) => foldOnce(v, acc)))
  }

  def foldOnce(fold: String, paper: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
    if(fold.startsWith("y")) {
      val y = Integer.parseInt(fold.split("=")(1))

      paper.keys.map(k => {
        if(k._2 < y) {
          k -> paper(k)
        } else {
          (k._1, 2*y - k._2) -> paper(k)
        }
      }).toMap.withDefaultValue('.')
    } else {
      val x = Integer.parseInt(fold.split("=")(1))

      paper.keys.map(k => {
        if(k._1 < x) {
          k -> paper(k)
        } else {
          (2*x - k._1, k._2) -> paper(k)
        }
      }).toMap.withDefaultValue('.')
    }
  }

  def buildMap(paper: List[String]): Map[(Int, Int), Char] = {
    paper.map(idx => {
      val s = idx.split(",")
      (Integer.parseInt(s(0)), Integer.parseInt(s(1))) -> '#'
    }).toMap.withDefaultValue('.')
  }

  def printMap(map: Map[(Int, Int), Char]): Unit = {
    val maxX = map.keys.maxBy(_._1)._1
    val maxY = map.keys.maxBy(_._2)._2

    for(j <- 0 to maxY) {
      for(i <- 0 to maxX) {
        print(map((i, j)))
      }
      println()
    }
  }
}
