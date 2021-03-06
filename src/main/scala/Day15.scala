import util.Util
import util.Util.{WeightedGraph, manhattan}

object Day15 {
  def main(args: Array[String]): Unit = {
    val map = Util.loadDayMap(15).view.mapValues(_ - '0').toMap

    //Part 1
    println(dist(map))

    //Part 2
    println(dist(extend(map)))
  }

  def dist(map: Map[(Int, Int), Int]): Int = Util.dijkstra(wg(map), (0, 0), maxCoords(map), (g, p) => g(p)).distance

  def wg(map: Map[(Int, Int), Int]): WeightedGraph[(Int, Int)] = map.map(entry => {
    entry._1 -> neighbors(map, entry._1).map(nb => (nb, map(nb))).toList
  })

  def neighbors(map: Map[(Int, Int), Int], p: (Int, Int)): Seq[(Int, Int)] = Util.vonNeumannNeighborhood.map(n => {
    (p._1 + n._1, p._2 + n._2)
  }).filter(p => map.contains(p))

  def extend(map: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    val d = maxCoords(map)
    val c = d._1 + 1
    val r = d._2 + 1
    (0 until 5 * c).flatMap(col => {
      (0 until 5 * r).map(row => {
        val n = map(col % c, row % r) + row/r + col/c
        (col, row) -> (if n > 9 then n - 9 else n)
      })
    }).toMap
  }

  def maxCoords(map: Map[(Int, Int), Int]): (Int, Int)=  {
    (map.keys.maxBy(_._1)._1, map.keys.maxBy(_._2)._2)
  }
}
