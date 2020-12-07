package adventofcode2020

import scala.io.Source

object DaySix {

  case class CustomsAnswers(groupSize: Int, answers: String)

  def readCustomsAnswers(name: String): Seq[CustomsAnswers] = {
    Source.fromResource(name)
      .mkString
      .split("(?m)^\\n")
      .map(s => CustomsAnswers(s.count(c => c == '\n' ), s.replaceAll("\\n", "")))
      .toSeq
  }

  def getTotalYesCountPerGroup(customsAnswers: Seq[CustomsAnswers]): Seq[Int] = {
    customsAnswers.map(ca => ca.answers.toSet.size)
  }

  def getUnanimousYesCountPerGroup(customsAnswers: Seq[CustomsAnswers]): Seq[Int] = {
    customsAnswers.map{ca =>
      ('a' to 'z').map(l => ca.answers.count(c => c == l)).filter(c => c == ca.groupSize)
    }.map(_.size)
  }

  def main(args: Array[String]): Unit = {
    val customsAnswers = readCustomsAnswers("day-6-part-1.txt")

    val totalYesCount = getTotalYesCountPerGroup(customsAnswers).sum

    println(s"Part One: $totalYesCount")

    val totalUnanimousYesCount = getUnanimousYesCountPerGroup(customsAnswers).sum

    println(s"Part Two: $totalUnanimousYesCount")
  }
}
