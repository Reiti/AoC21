import util.Util

import scala.collection.mutable
import scala.collection.mutable.HashMap

object Day9 {
  def main(args: Array[String]): Unit = {
    val map = Util.loadDayLines(9).map(_.split("").map(i => Integer.parseInt(i)).toList)

    //Part 1
    println(sumRisks(map))

    //Part 2
    println(basins(map).values.groupBy(identity).map(_._2.size).toList.sorted.reverse.slice(0, 3).product)
  }

  def sumRisks(map: List[List[Int]]): Int = {
    map.indices.flatMap(i => {
      map(i).indices.map(j => {
        val curr = map(i)(j)
        if (
          ((i - 1) >= 0 && map(i - 1)(j) <= curr) ||
          ((i + 1) < map.length && (map(i + 1)(j) <= curr)) ||
          ((j + 1) < map(i).length && (map(i)(j + 1) <= curr)) ||
          ((j - 1) >= 0 && map(i)(j - 1) <= curr)
        ) {
          0
        } else {
          curr + 1
        }
      })
    }).sum
  }

  def basins(map: List[List[Int]]): Map[(Int, Int), Int] = {
    val acc: mutable.HashMap[(Int, Int), Int] = mutable.HashMap()
    map.indices.foreach(i => {
      map(i).indices.foreach(j => {
        if(map(i)(j) != 9) {
          val bas = belongsTo(acc, i, j).distinct
          if(bas.size > 1) {
            val c = acc.toList.filter(b => bas.contains(b._2))
            c.foreach(e => acc.remove(e._1))
            val min = c.map(_._2).min
            acc.addAll(c.map(e => (e._1, min)))
            acc.update((i, j), min)
          } else {
            bas match {
              case x :: xs => acc.update((i, j), x)
              case Nil =>
                val maxB = if acc.values.isEmpty then 0 else acc.values.max
                acc.update((i, j), maxB + 1)
            }
          }
        }
      })
    })
    acc.toMap
  }

  def belongsTo(acc: mutable.HashMap[(Int, Int), Int], i: Int, j: Int): List[Int] = {
    (-1 to 1).flatMap(di =>
      (-1 to 1).map(dj =>
        if(di == 0 || dj == 0) {
          acc.get(i + di, j + dj)
        } else {
          None
        }).filter(_.nonEmpty).map(_.get)).toList
  }
}
