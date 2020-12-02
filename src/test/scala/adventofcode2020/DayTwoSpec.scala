package adventofcode2020

import adventofcode2020.DayTwo.{Password, isValidPasswordPartOne, isValidPasswordPartTwo}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DayTwoSpec extends AnyFlatSpecLike with Matchers{

  it should "read in a file of password policies and passwords" in {
    // Given
    val expected = Seq(
      Password(1, 3, 'a', "abcde"),
      Password(1, 3, 'b', "cdefg"),
      Password(2, 9, 'c', "ccccccccc")
    )

    // When
    val actual = DayTwo.readPasswords("day-2-part-1-test.txt")
    actual shouldBe expected
  }

  it should "return true if a password is valid - part 1" in {
    DayTwo.isValidPasswordPartOne(Password(1, 3, 'a', "abcde")) shouldBe true
    DayTwo.isValidPasswordPartOne(Password(2, 9, 'c', "ccccccccc")) shouldBe true
  }

  it should "return false if a password is not valid - part 1" in {
    DayTwo.isValidPasswordPartOne(Password(1, 3, 'b', "cdefg")) shouldBe false
  }

  it should "return the number of valid passwords - part 1" in {
    // Given
    val passwords = Seq(
      Password(1, 3, 'a', "abcde"),
      Password(1, 3, 'b', "cdefg"),
      Password(2, 9, 'c', "ccccccccc")
    )

    // When
    val actual = DayTwo.numberOfCorrectPasswords(passwords, isValidPasswordPartOne)

    // Then
    actual shouldBe 2
  }

  it should "return true if a password is valid - part 2" in {
    DayTwo.isValidPasswordPartTwo(Password(1, 3, 'a', "abcde")) shouldBe true
  }

  it should "return false if a password is not valid - part 2" in {
    DayTwo.isValidPasswordPartTwo(Password(1, 3, 'b', "cdefg")) shouldBe false
    DayTwo.isValidPasswordPartTwo(Password(2, 9, 'c', "ccccccccc")) shouldBe false
  }

  it should "return the number of valid passwords - part 2" in {
    // Given
    val passwords = Seq(
      Password(1, 3, 'a', "abcde"),
      Password(1, 3, 'b', "cdefg"),
      Password(2, 9, 'c', "ccccccccc")
    )

    // When
    val actual = DayTwo.numberOfCorrectPasswords(passwords, isValidPasswordPartTwo)

    // Then
    actual shouldBe 1
  }

}
