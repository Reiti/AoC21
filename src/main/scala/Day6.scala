import util.Util

import scala.collection.mutable

object Day6 {
  def main(args: Array[String]): Unit = {
    val fish = Util.loadDay(6).split(",").map(_.toInt)

    //Part 1
    println((1 to 80).foldLeft(fish)((f, _) => step(f)).length)

    //Part 2
    println(fish.map(t => offspring(t, 256)).sum + fish.length)
  }

  def step(fish: Array[Int]): Array[Int] = {
    fish.map { case 0 => 6; case x => x - 1 } ++ List.fill(fish.count(_ == 0))(8)
  }

  val memo = mutable.HashMap.empty[(BigInt, BigInt), BigInt]

  def offspring(time: BigInt, days: BigInt): BigInt = {
    if(memo.contains((time, days))) {
      memo((time, days))
    } else {
     if(days <= time) {
       0
     } else {
       val r = 1 + offspring(6, days - time - 1) + offspring(8, days - time - 1)
       memo.put((time, days), r)
       r
     }
    }
  }
}
