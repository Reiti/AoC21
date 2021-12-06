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

  def offspringSingleFish(time: BigInt, days: BigInt): BigInt = {
    (days - time - 1)/7 + (if days > time then 1 else 0)
  }

  def offspring(time: BigInt, days: BigInt): BigInt = {
    if(memo.contains((time, days))) {
      memo((time, days))
    } else {
      val o = offspringSingleFish(time, days)
      if (days - time - 1 < 0)
        0
      else
        val r = o + offspring(8, days - time - 1) +
          (1 to o.toInt).map(delay => offspring(8, (days - time - 1) - delay * 7)).sum
        memo.put((time, days), r)
        r
    }
  }
}
