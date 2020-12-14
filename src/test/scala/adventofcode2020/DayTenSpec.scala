package adventofcode2020

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DayTenSpec extends AnyFlatSpecLike with Matchers {

  it should "find the joltage difference between adjacent adapters" in {
    // Given
    val adapters = Seq(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
    val expected = Map(1 -> 7, 3 -> 5)

    // When
    val actual = DayTen.findJoltDifferences(adapters)

    // Then
    actual shouldBe expected
  }

  it should "get the difference in jolts between ordered adapters" in {
    // Given
    val adapters = Seq(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
    val expected = Seq(1, 3, 1, 1, 1, 3, 1, 1, 3, 1, 3, 3)

    // When
    val actual = DayTen.differences(adapters)

    // Then
    actual shouldBe expected
  }

  it should "get all the lengths of consecutive 1s" in {
    // Given
    val lengths = Seq(1, 3, 1, 1, 1, 3, 1, 1, 3, 1, 3, 3)
    val expected = Seq(1,3,2,1)
    // When
    val actual = DayTen.consecutive1sLengths(lengths)

    // Then
    actual shouldBe expected
  }

  it should "get the number of possible valid adapter combinations" in {
    // Given
    val adapters = Seq(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)

    // When
    val actual = DayTen.findNumberOfPermutations(adapters)

    // Then
    actual shouldBe 8
  }

}
