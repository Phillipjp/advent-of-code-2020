package adventofcode2020

import adventofcode2020.DayThree.Slope
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DayThreeSpec extends AnyFlatSpecLike with Matchers{

  it should "read in a map" in {
    // Given
    val expected = Seq(
      Seq('.', '.', '#', '#', '.', '.', '.', '.', '.', '.', '.'),
      Seq('#', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.'),
      Seq('.', '#', '.', '.', '.', '.', '#', '.', '.', '#', '.'),
      Seq('.', '.', '#', '.', '#', '.', '.', '.', '#', '.', '#'),
      Seq('.', '#', '.', '.', '.', '#', '#', '.', '.', '#', '.'),
      Seq('.', '.', '#', '.', '#', '#', '.', '.', '.', '.', '.'),
      Seq('.', '#', '.', '#', '.', '#', '.', '.', '.', '.', '#'),
      Seq('.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '#'),
      Seq('#', '.', '#', '#', '.', '.', '.', '#', '.', '.', '.'),
      Seq('#', '.', '.', '.', '#', '#', '.', '.', '.', '.', '#'),
      Seq('.', '#', '.', '.', '#', '.', '.', '.', '#', '.', '#')
    )

    // When
    val actual = DayThree.readMap("day-3-part-1-test.txt")

    // Then
    actual shouldBe expected
  }

  it should "count the trees travelled past" in {
    // Given
    val map = Seq(
      Seq('.', '.', '#', '#', '.', '.', '.', '.', '.', '.', '.'),
      Seq('#', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.'),
      Seq('.', '#', '.', '.', '.', '.', '#', '.', '.', '#', '.'),
      Seq('.', '.', '#', '.', '#', '.', '.', '.', '#', '.', '#'),
      Seq('.', '#', '.', '.', '.', '#', '#', '.', '.', '#', '.'),
      Seq('.', '.', '#', '.', '#', '#', '.', '.', '.', '.', '.'),
      Seq('.', '#', '.', '#', '.', '#', '.', '.', '.', '.', '#'),
      Seq('.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '#'),
      Seq('#', '.', '#', '#', '.', '.', '.', '#', '.', '.', '.'),
      Seq('#', '.', '.', '.', '#', '#', '.', '.', '.', '.', '#'),
      Seq('.', '#', '.', '.', '#', '.', '.', '.', '#', '.', '#')
    )

    val slope = Slope(3, 1)

    val expected = 7

    // When
    val actual = DayThree.countTreesTraveledPast(map, slope)

    // Then
    actual shouldBe expected
  }

}
