import util.Util
import util.Util.WeightedGraph

import scala.annotation.tailrec

object Day15 {
  def main(args: Array[String]): Unit = {
    val map = Util.loadDayMap(15).view.mapValues(_ - '0').toMap

    //Part 1
    println(Util.dijkstra(wg(map), (0, 0), dim(map), (g, p) => g(p)))

    val ext = extend(map)

    //Part 2
    println(Util.dijkstra(wg(ext), (0, 0), dim(ext), (g, p) => g(p)))
  }

  def wg(map: Map[(Int, Int), Int]): WeightedGraph[(Int, Int)] = map.map(entry => {
    entry._1 -> neighbors(map, entry._1).map(nb => (nb, map(nb))).toList
  })

  def neighbors(map: Map[(Int, Int), Int], p: (Int, Int)): Seq[(Int, Int)] = Util.vonNeumannNeighborhood.map(n => {
    (p._1 + n._1, p._2 + n._2)
  }).filter(p => map.contains(p))

  def extend(map: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    val d = dim(map)
    val c = d._1 + 1
    val r = d._2 + 1
    (0 until 5 * c).flatMap(col => {
      (0 until 5 * r).map(row => {
        val addR = row/r
        val addC = col/c

        val n = map(col % c, row % r) + addR + addC

        (col, row) -> (if n > 9 then n - 9 else n)
      })
    }).toMap
  }

  def dim(map: Map[(Int, Int), Int]): (Int, Int)=  {
    (map.keys.maxBy(_._1)._1, map.keys.maxBy(_._2)._2)
  }
}
