import util.Util

import scala.annotation.tailrec

object Day25:
  def main(args: Array[String]): Unit =
    val input = Util.loadDayMap(25)
    val maxY = input.keys.maxBy(_._1)._1 + 1
    val maxX = input.keys.maxBy(_._2)._2 + 1

    //Part 1
    println(sim(input, maxY, maxX))
  end main

  def east(cucumbers: Map[(Int, Int), Char], maxX: Int): Map[(Int, Int), Char] =
    cucumbers.map({case (k, v) =>
      if v == '.' && cucumbers(k._1, posMod(k._2 - 1, maxX)) == '>' then
          k -> '>'
      else if v == '>' && cucumbers(k._1, posMod(k._2 + 1, maxX)) == '.' then
          k -> '.'
      else
        k -> v
    })

  def south(cucumbers: Map[(Int, Int), Char], maxY: Int): Map[(Int, Int), Char] =
    cucumbers.map({case (k, v) =>
      if v == '.' && cucumbers(posMod(k._1 - 1, maxY), k._2) == 'v' then
        k -> 'v'
      else if v == 'v' && cucumbers(posMod(k._1 + 1, maxY), k._2) == '.' then
        k -> '.'
      else
        k -> v
    })

  def posMod(v: Int, m: Int): Int = (v%m + m) % m

  @tailrec
  def sim(map: Map[(Int, Int), Char], maxY: Int, maxX: Int, steps: Int = 0): Int =
    val nextMap = south(east(map, maxX), maxY)
    if nextMap == map then
      steps + 1
    else
      sim(nextMap, maxY, maxX, steps + 1)