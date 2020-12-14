package adventofcode2020

import scala.io.Source

object DayTen {

  def findJoltDifferences(adapters: Seq[Int]): Map[Int, Int] = {
    ((adapters :+ 0) :+ adapters.max + 3).sorted.sliding(2,1).toSeq.groupBy(l => l.last - l.head).mapValues(_.size)
  }

  def differences(adapters: Seq[Int]): Seq[Int] = {
    ((adapters :+ 0) :+ adapters.max + 3).sorted.sliding(2,1).toSeq.map(l => l.last - l.head)
  }

  def consecutive1sLengths(diffs: Seq[Int]): Seq[Int] = {
    val split = diffs.mkString("").split("3")
    split.map(_.length)
  }

  private val tribonacciSequence = Seq(1L, 1L, 2L, 4L, 7L, 13L, 24L, 44L, 81L, 149L)

  def findNumberOfPermutations(adapters: Seq[Int]): Long = {
    val diffs = differences(adapters)
    val lengths = consecutive1sLengths(diffs).filter(l => l != 0)
    lengths.map(n => tribonacciSequence(n)).product
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day-10-part-1.txt").getLines().toSeq.map(_.toInt)
    val joltDifferences = findJoltDifferences(input)
    val part1 = joltDifferences(1) * joltDifferences(3)
    println(s"Part One: $part1")

    val part2 = findNumberOfPermutations(input)
    println(s"Part Two: $part2")
  }

}
