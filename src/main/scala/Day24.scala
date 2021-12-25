import util.Util

import scala.annotation.tailrec

object Day24:
  def main(args: Array[String]): Unit =
    val steps = Util.loadDayLines(24).map(_.split(" ").toList).grouped(18).toList

    //Part 1
    println(stepWise(steps, max = true))

    //Part 2
    println(stepWise(steps, max = false))
  end main

  @tailrec
  def execute(program: List[List[String]], acc: Map[String, Long], input: List[Long]): Map[String, Long] = program match
    case x :: xs => x match
      case "inp" :: a :: Nil => execute(xs, acc.updated(a, input.head), input.tail)
      case instr :: a :: b :: Nil =>
        val x = acc(a)
        val y = acc.getOrElse(b, b.toLong)
        instr match
          case "add" => execute(xs, acc.updated(a, x + y), input)
          case "mul" => execute(xs, acc.updated(a, x * y), input)
          case "div" => execute(xs, acc.updated(a, x / y), input)
          case "mod" => execute(xs, acc.updated(a, x % y), input)
          case "eql" => execute(xs, acc.updated(a, if x == y then 1L else 0L), input)
      case _ => throw new RuntimeException("Unknown Instruction!")
    case _ => acc

  def reduceStates(states: List[(Long, Long)], max: Boolean): List[(Long, Long)] =
    states.groupBy(_._2).map(g =>
      if max then
        (g._2.maxBy(_._1)._1, g._1)
      else
        (g._2.minBy(_._1)._1, g._1)
    ).toList

  @tailrec
  def stepWise(steps: List[List[List[String]]], state: List[(Long, Long)] = List((0L, 0L)), max: Boolean): Long = steps match
    case x :: xs =>
      val st = state.flatMap(s =>
        (1 to 9).map(n =>
          (s._1 * 10 + n.toLong, execute(x, Map("w" -> 0L, "x" -> 0L, "y" -> 0L, "z" -> s._2), List(n.toLong))("z"))
        )
      )
      stepWise(xs, reduceStates(st, max).filter(z => z._2 <= 26L*26L*26L*26L*26L), max)
    case _ =>
      if max then
        state.filter(_._2 == 0).maxBy(_._1)._1
      else
        state.filter(_._2 == 0).minBy(_._1)._1