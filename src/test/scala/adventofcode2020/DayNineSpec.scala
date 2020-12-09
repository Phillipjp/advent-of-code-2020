package adventofcode2020

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DayNineSpec extends AnyFlatSpecLike with Matchers {

  it should "read the puzzle input" in {
    // Given
    val expected =  Seq(
      35l, 20l, 15l, 25l, 47l, 40l, 62l, 55l, 65l, 95l, 102l, 117l, 150l, 182l, 127l, 219l, 299l, 277l, 309l, 576l
    )

    // When
    val actual = DayNine.readInput("day-9-part-1-test.txt")

    // Then
    actual shouldBe expected
  }

  it should "find the exception to the rule" in {
    // Given
    val input =  Seq(
      35l, 20l, 15l, 25l, 47l, 40l, 62l, 55l, 65l, 95l, 102l, 117l, 150l, 182l, 127l, 219l, 299l, 277l, 309l, 576l
    )
    val preambleSize = 5

    // When
    val actual = DayNine.partOne(input, preambleSize)

    // Then
    actual shouldBe 127

  }

  it should "find the sum of the smallest and larget numbers in the sequence that adds to the invalid number" in {
    // Given
    val input =  Seq(
      35l, 20l, 15l, 25l, 47l, 40l, 62l, 55l, 65l, 95l, 102l, 117l, 150l, 182l, 127l, 219l, 299l, 277l, 309l, 576l
    )

    val invalidNumber = 127

    // When
    val actual = DayNine.partTwo(input, invalidNumber)

    // Then
    actual shouldBe 62

  }
}
