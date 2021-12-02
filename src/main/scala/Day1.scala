import util.Util


object Day1 {
  def main(args: Array[String]): Unit = {
    val depths = Util.loadDayInts(1)

    //Part 1
    println(depths.sliding(2).count(d => d.head < d(1)))

    //Part 2
    println(depths.sliding(3).map(_.sum).sliding(2).count(d => d.head < d(1)))
  }
}
