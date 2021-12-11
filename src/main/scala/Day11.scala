import util.Util

import scala.collection.{mutable, *}

object Day11 {
  def main(args: Array[String]): Unit = {
    val octopuses = Util.loadDayLines(11).toArray.map(_.split("").map(Integer.parseInt))

    val map = new mutable.HashMap[(Int, Int), Int]()

    for(i <- 0 until 10) {
      for(j <- 0 until 10) {
        map.update((i, j), octopuses(i)(j))
      }
    }

    val res = solve(map, 100)

    //Part 1
    println(res._1)

    //Part 2
    println(res._2)
  }

  def solve(map: mutable.HashMap[(Int, Int), Int], stepTarget: Int): (Int, Int) = {
    var flashes = 0
    var steps = 0
    var flashRet = 0
    while(true) {
      val flashed = new mutable.HashSet[(Int, Int)]()
      val toFlash = new mutable.HashSet[(Int, Int)]()

      if(steps == stepTarget) {
        flashRet = flashes
      }

      map.mapValuesInPlace((k, v) => v + 1)
      toFlash.addAll(map.filter(_._2 > 9).keys)

      while (toFlash.nonEmpty) {
        val curr = toFlash.head
        if (!flashed.contains(curr)) {
          flashed.add(curr)
          map.update(curr, 0)
          flashes = flashes + 1

          (-1 to 1).map(x => {
            (-1 to 1).map(y => {
                val pos = (curr._1 + x, curr._2 + y)
                if(map.contains(pos)) {
                  if(!flashed.contains(pos)) {
                    map.update(pos, map(pos) + 1)
                    if (map(pos) > 9) {
                      toFlash.add(pos)
                    }
                  }
                }
            })
          })
        } else {
          toFlash.remove(curr)
        }
      }
      steps = steps + 1

      if(flashed.size == 100) {
        return (flashRet, steps)
      }
    }
    (-1, -1)
  }
}
