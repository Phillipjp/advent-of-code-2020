package adventofcode2020

import scala.io.Source

object DayTwo {

  case class Password(min: Int, max: Int, character: Char, password: String)

  def readPasswords(name: String): Seq[Password] = {
    Source.fromResource(name)
      .getLines()
      .map{line =>
        val separated: Array[String] = line.split(" ")
        val range = separated.head.split("-")
        Password(range.head.toInt, range.last.toInt, separated(1).charAt(0), separated.last)
      }
      .toSeq
  }

  def isValidPasswordPartOne(password: Password): Boolean = {
    val p = password.password.filter(c => c == password.character)
    p.length >= password.min && p.length <= password.max
  }

  def isValidPasswordPartTwo(password: Password): Boolean = {
    password.password.charAt(password.min - 1) == password.character && password.password.charAt(password.max - 1) != password.character ||
    password.password.charAt(password.min - 1) != password.character && password.password.charAt(password.max - 1) == password.character
  }

  def numberOfCorrectPasswords(passwords: Seq[Password], isValidPassword: (Password => Boolean)): Int = {
    passwords.count(isValidPassword)
  }

  def main(args: Array[String]): Unit = {
    val passwords = readPasswords("day-2-part-1.txt")

    val part1 = numberOfCorrectPasswords(passwords, isValidPasswordPartOne)
    val part2 = numberOfCorrectPasswords(passwords, isValidPasswordPartTwo)

    println(s"Part One: $part1")
    println(s"Part Two: $part2")
  }

}
