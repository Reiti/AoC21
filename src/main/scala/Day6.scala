import util.Util

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

  lazy val offspring: ((BigInt, BigInt)) => BigInt = Util.memoize {
    case (t, d) if d <= t => 0
    case (t, d) => 1 + offspring(6, d - t - 1) + offspring(8, d - t - 1)
  }
}
