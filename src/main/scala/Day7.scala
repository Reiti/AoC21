import util.Util

object Day7 {
  def main(args: Array[String]): Unit = {
    val crabs = Util.loadDay(7).split(",").map(_.toInt)

    //Part 1
    println((0 to crabs.max).map(p => crabs.map(c => Math.abs(c - p)).sum).min)

    //Part 2
    println((0 to crabs.max).map(p => crabs.map(c => {
      val diff = Math.abs(c - p)
      diff*(diff+1)/2
    }).sum).min)
  }
}
