
import util.Util

object Day16 {
  case class Packet(version: Int, typeId: Int, subPackets: List[Packet], value: Long)

  def main(args: Array[String]): Unit = {
    val transmission = parseOperator(Util.loadDay(16).split("").map(decodeHex).mkString)._1

    //Part 1
    println(versionSum(transmission))

    //Part 2
    println(calcValue(transmission))
  }

  def decodeHex(hex: String): String = {
    val b = Integer.toBinaryString(Integer.parseInt(hex, 16))
    b.reverse.padTo(4, '0').reverse
  }

  def parseLiteral(binary: String): (Packet, Int) = {
    val version = Integer.parseInt(binary.take(3), 2)
    val typeId = Integer.parseInt(binary.slice(3, 6), 2)
    var b = binary.drop(6)

    val nums = scala.collection.mutable.ArrayBuffer[String]()

    while(!b.startsWith("0")) {
      nums.addOne(b.slice(1, 5))
      b = b.drop(5)
    }

    nums.addOne(b.slice(1, 5))

    val num = nums.mkString

    (Packet(version, typeId, List(), java.lang.Long.parseLong(num, 2)), num.length + nums.size + 6)
  }

  def parseOperator(binary: String): (Packet, Int) = {
    val version = Integer.parseInt(binary.take(3), 2)
    val typeId = Integer.parseInt(binary.slice(3, 6), 2)
    val sizeType = Integer.parseInt(binary.slice(6, 7), 2)
    if(sizeType == 0) {
      val bits = Integer.parseInt(binary.slice(7, 22), 2)
      val nextFull = ((Math.ceil((bits + 22).toDouble)/4)*4).toInt
      val nextBytes = binary.slice(22, nextFull)

      (Packet(version, typeId, parseSubPackets(nextBytes), 0), nextFull)
    } else {
      val count = Integer.parseInt(binary.slice(7, 18), 2)
      val (packets, len) = parseSubPackets(binary.drop(18), count)

      (Packet(version, typeId, packets, 0), len + 18)
    }
  }

  def parseSubPackets(binary: String): List[Packet] = {
    var bin = binary
    val packets = scala.collection.mutable.ArrayBuffer[Packet]()
    while(bin.exists(_ != '0') && bin.nonEmpty) {
      val version = Integer.parseInt(bin.take(3), 2)
      val typeId = Integer.parseInt(bin.slice(3, 6), 2)

      if(typeId == 4) {
        val (lit, size) = parseLiteral(bin)
        bin = bin.drop(size)
        packets.addOne(lit)
      } else {
        val (op, size) = parseOperator(bin)
        bin = bin.drop(size)
        packets.addOne(op)
      }
    }

    packets.toList
  }

  def parseSubPackets(binary: String, count: Int): (List[Packet], Int) = {
    var bin = binary
    var c = count
    val packets = scala.collection.mutable.ArrayBuffer[Packet]()
    var l = 0

    while(c != 0 && bin.nonEmpty) {
      val version = Integer.parseInt(bin.take(3), 2)
      val typeId = Integer.parseInt(bin.slice(3, 6), 2)

      if(typeId == 4) {
        val (lit, size) = parseLiteral(bin)
        bin = bin.drop(size)
        l += size
        c = c - 1
        packets.addOne(lit)
      } else {
        val (op, size) = parseOperator(bin)
        bin = bin.drop(size)
        l += size
        c = c - 1
        packets.addOne(op)
      }

    }

    (packets.toList, l)
  }

  def versionSum(operator: Packet): Int = {
    operator.version + operator.subPackets.map(versionSum).sum
  }

  def calcValue(operator: Packet): Long = operator.typeId match {
    case 0 => operator.subPackets.map(calcValue).sum
    case 1 => operator.subPackets.map(calcValue).product
    case 2 => operator.subPackets.map(calcValue).min
    case 3 => operator.subPackets.map(calcValue).max
    case 4 => operator.value
    case 5 => if calcValue(operator.subPackets.head) > calcValue(operator.subPackets.last) then 1 else 0
    case 6 => if calcValue(operator.subPackets.head) < calcValue(operator.subPackets.last) then 1 else 0
    case 7 => if calcValue(operator.subPackets.head) == calcValue(operator.subPackets.last) then 1 else 0
  }
}
