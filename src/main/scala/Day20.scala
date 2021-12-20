import util.Util

object Day20 {
  def main(args: Array[String]): Unit = {
    val input = Util.loadDayLines(20).toArray
    val algo = input.head
    val map = Util.parseMap(input.drop(2))

    //Part 1
    println(step(step(map, algo), algo).values.count(_ == '#'))

    //Part 2
    println((0 until 50).foldLeft(map)((acc, _) => step(acc, algo)).values.count(_ == '#'))
  }

  def step(map: Map[(Int, Int), Char], algo: String): Map[(Int, Int), Char] = {
    val (miny, minx) = min(map)
    val (maxy, maxx) = max(map)

    val swapDefault = algo.head != '.'

    (for {
      i <- miny-1 to maxy+1
      j <- minx-1 to maxx+1
    } yield (i, j) -> decode(map, (i, j), algo)).toMap.withDefaultValue(if swapDefault && map.default((0, 0)) == '.' then '#' else '.')
  }

  def decode(map: Map[(Int, Int), Char], p: (Int, Int), algo: String): Char = {
    val s = for {
      i <- -1 to 1
      j <- -1 to 1
    } yield map((p._1 + i, p._2 + j))

    val idx = Integer.parseInt(s.mkString.replace('#', '1').replace('.', '0'), 2)

    algo(idx)
  }

  def min(map: Map[(Int, Int), Char]): (Int, Int) = {
    (map.keys.minBy(_._1)._1, map.keys.minBy(_._2)._2)
  }

  def max(map: Map[(Int, Int), Char]): (Int, Int) = {
    (map.keys.maxBy(_._1)._1, map.keys.maxBy(_._2)._2)
  }
}
