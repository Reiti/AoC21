import util.Util

object Day8 {
  def main(args: Array[String]): Unit = {
    val signals = Util.loadDayLines(8).map(_.split("\\|"))
    val inputs = signals.map(_(0).trim.split(" ").map(_.trim).toList)
    val outputs = signals.map(_(1).trim.split(" ").map(_.trim).toList)

    //Part 1
    println(outputs.map(_.count(str => str.length == 2 || str.length == 3 ||  str.length == 4 || str.length == 7)).sum)

    //Part 2
    println(calculateOutputs(inputs, outputs).map(_.mkString.toInt).sum)
  }

  def calculateOutputs(inputs: List[List[String]], outputs: List[List[String]]): Seq[List[Int]] = {
    val mappings = inputs.map(in => {
      nine(in.filter(str => str.length != 2 && str.length != 3 &&  str.length != 4 && str.length != 7), initMapping(in))
    })

    (mappings zip outputs).map(v =>
      v._2.map(o =>
        v._1.find(entry => entry._2.toSet == o.toSet).get._1
      )
    )
  }

  def initMapping(inputs: List[String]): Map[Int, String] = {
    Map(1 -> inputs.find(_.length==2).get,
        4 -> inputs.find(_.length==4).get,
        7 -> inputs.find(_.length==3).get,
        8 -> inputs.find(_.length==7).get)
  }

  def nine(inputs: List[String], mapping: Map[Int, String]): Map[Int, String] = {
    val n = inputs.find(str => str.length == 6 && mapping(4).toSet.subsetOf(str.toSet)).get

    zero(inputs.filter(_ != n), mapping.updated(9, n))
  }

  def zero(inputs: List[String], mapping: Map[Int, String]): Map[Int, String] = {
    val z = inputs.find(str => str.length == 6 && mapping(1).toSet.subsetOf(str.toSet)).get

    six(inputs.filter(_ != z), mapping.updated(0, z))
  }

  def six(inputs: List[String], mapping: Map[Int, String]): Map[Int, String] = {
    val s = inputs.find(str => str.length == 6).get

    five(inputs.filter(_ != s), mapping.updated(6, s))
  }

  def five(inputs: List[String], mapping: Map[Int, String]): Map[Int, String] = {
    val f = inputs.find(str => str.length == 5 && str.toSet.subsetOf(mapping(6).toSet)).get

    three(inputs.filter(_ != f), mapping.updated(5, f))
  }

  def three(inputs: List[String], mapping: Map[Int, String]): Map[Int, String] = {
    val t = inputs.find(str => str.length == 5 && mapping(1).toSet.subsetOf(str.toSet)).get

    two(inputs.filter(_ != t), mapping.updated(3, t))
  }

  def two(inputs: List[String], mapping: Map[Int, String]): Map[Int, String] = {
    val t = inputs.head

    mapping.updated(2, t)
  }
}
