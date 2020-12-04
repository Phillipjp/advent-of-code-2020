package adventofcode2020

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DayFourSpec extends AnyFlatSpecLike with Matchers{

  it should "read all the available fields for a passport" in {
    // Given
    val expected = Seq(
      Seq("ecl:gry", "pid:860033327", "eyr:2020", "hcl:#fffffd", "byr:1937", "iyr:2017", "cid:147", "hgt:183cm"),
      Seq("iyr:2013", "ecl:amb", "cid:350", "eyr:2023", "pid:028048884", "hcl:#cfa07d", "byr:1929"),
      Seq("hcl:#ae17e1", "iyr:2013", "eyr:2024", "ecl:brn", "pid:760753108", "byr:1931", "hgt:179cm"),
      Seq("hcl:#cfa07d", "eyr:2025", "pid:166559648", "iyr:2011", "ecl:brn", "hgt:59in")
    )


    // When
    val actual = DayFour.readPassports("day-4-part-1-test.txt")

    // Then
    actual shouldBe expected

  }

  it should "return true when a passport contains all the required fields" in {
    // Given
    val passport1 = Seq("ecl:gry", "pid:860033327", "eyr:2020", "hcl:#fffffd", "byr:1937", "iyr:2017", "cid:147", "hgt:183cm")
    val passport2 = Seq("hcl:#ae17e1", "iyr:2013", "eyr:2024", "ecl:brn", "pid:760753108", "byr:1931", "hgt:179cm")

    // When, Then
    DayFour.allRequiredFieldsPresent(passport1) shouldBe true
    DayFour.allRequiredFieldsPresent(passport2) shouldBe true
  }

  it should "return false when a passport is missing some of the required fields" in {
    // Given
    val passport1 = Seq("iyr:2013", "ecl:amb", "cid:350", "eyr:2023", "pid:028048884", "hcl:#cfa07d", "byr:1929")
    val passport2 = Seq("hcl:#cfa07d", "eyr:2025", "pid:166559648", "iyr:2011", "ecl:brn", "hgt:59in")

    // When, Then
    DayFour.allRequiredFieldsPresent(passport1) shouldBe false
    DayFour.allRequiredFieldsPresent(passport2) shouldBe false
  }

  it should "count the number of valid passports - Part 1" in {

    // Given
    val passports = Seq(
      Seq("ecl:gry", "pid:860033327", "eyr:2020", "hcl:#fffffd", "byr:1937", "iyr:2017", "cid:147", "hgt:183cm"),
      Seq("iyr:2013", "ecl:amb", "cid:350", "eyr:2023", "pid:028048884", "hcl:#cfa07d", "byr:1929"),
      Seq("hcl:#ae17e1", "iyr:2013", "eyr:2024", "ecl:brn", "pid:760753108", "byr:1931", "hgt:179cm"),
      Seq("hcl:#cfa07d", "eyr:2025", "pid:166559648", "iyr:2011", "ecl:brn", "hgt:59in")
    )

    // When
    val actual = DayFour.countValidPassports(passports, DayFour.allRequiredFieldsPresent)

    // Then
    actual shouldBe 2
  }

}
