package adventofcode2020

import scala.io.Source

object DayFour {

  def readPassports(name: String): Seq[Seq[String]] = {
    val pattern ="([a-z]{3}:)([^\\s]+)".r
    Source.fromResource(name)
      .mkString
      .split("(?m)^\\n")
      .map(line => pattern.findAllIn(line).mkString(" ").split(" ").toSeq)
      .toSeq
  }

  private val requiredFields = Seq(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
  )

  def allRequiredFieldsPresent(passport: Seq[String]): Boolean = {
    val pattern ="""[a-z]{3}:""".r

    val passportFields =  pattern
      .findAllIn(passport.mkString)
      .mkString
      .split(":")
      .toSeq

    requiredFields.forall(field => passportFields.contains(field))
  }

  def allRequiredFieldsArePresentAndCorrect(passport: Seq[String]): Boolean = {
    allRequiredFieldsPresent(passport) && {
      passport.forall {
        case f if f.startsWith("byr") => isValidBirthYear(f.split(":").last.toInt)
        case f if f.startsWith("iyr") => isValidIssueYear(f.split(":").last.toInt)
        case f if f.startsWith("eyr") => isValidExpirationYear(f.split(":").last.toInt)
        case f if f.startsWith("hgt") => isValidHeight(f.split(":").last)
        case f if f.startsWith("hcl") => isvalidHairColour(f.split(":").last)
        case f if f.startsWith("ecl") => isValidEyeColour(f.split(":").last)
        case f if f.startsWith("pid") => isValidPassportNumber(f.split(":").last)
        case f if f.startsWith("cid") => true
      }
    }
  }

  def isValidBirthYear(byr: Int): Boolean = inValidRange(byr, 1920, 2020)

  def isValidIssueYear(iyr: Int): Boolean = inValidRange(iyr, 2010, 2020)

  def isValidExpirationYear(eyr: Int): Boolean = inValidRange(eyr, 2020, 2030)

  def isValidHeight(hgt: String): Boolean = {
    hgt match {
      case h if h.endsWith("cm") => inValidRange(h.split("c").head.toInt, 150, 193)
      case h if h.endsWith("in") => inValidRange(h.split("i").head.toInt, 59, 76)
      case _ => false
    }
  }

  def isvalidHairColour(hcl: String): Boolean = hcl.matches("#(([0-9]|[a-f]){6})")

  def isValidEyeColour(ecl: String): Boolean = {
    ecl match {
      case "amb" => true
      case "blu" => true
      case "brn" => true
      case "gry" => true
      case "grn" => true
      case "hzl" => true
      case "oth" => true
      case _ => false
    }
  }

  def isValidPassportNumber(pid: String): Boolean = pid.matches("[0-9]{9}")

  private def inValidRange(n: Int, lower: Int, upper: Int): Boolean = n >= lower && n <= upper

  def countValidPassports(passports: Seq[Seq[String]], condition: (Seq[String] => Boolean)): Int = {
    passports.count(passport => condition(passport))
  }

  def main(args: Array[String]): Unit = {

    val passports = readPassports("day-4-part-1.txt")
    val validPassportsCount = countValidPassports(passports, allRequiredFieldsPresent)

    println(s"Part One: $validPassportsCount")

    val part2 = countValidPassports(passports, allRequiredFieldsArePresentAndCorrect)

    println(s"Part Two: $part2")

  }

}
