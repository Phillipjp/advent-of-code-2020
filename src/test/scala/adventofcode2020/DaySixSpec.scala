package adventofcode2020

import adventofcode2020.DaySix.CustomsAnswers
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DaySixSpec extends AnyFlatSpecLike with Matchers {

  it should "read in the customs answers" in {
    // Given
    val expected = Seq(
      CustomsAnswers(1, "abc"),
      CustomsAnswers(3, "abc"),
      CustomsAnswers(2, "abac"),
      CustomsAnswers(4, "aaaa"),
      CustomsAnswers(1, "b")
      )

    // When
    val actual = DaySix.readCustomsAnswers("day-6-part-1-test.txt")

    // Then
    actual shouldBe expected
  }

  it should "Get the total yes count per group" in {
    // Given
    val answers = Seq(
      CustomsAnswers(1, "abc"),
      CustomsAnswers(3, "abc"),
      CustomsAnswers(2, "abac"),
      CustomsAnswers(4, "aaaa"),
      CustomsAnswers(1, "b")
    )

    val expected = Seq(3, 3, 3, 1, 1)

    // When
    val actual = DaySix.getTotalYesCountPerGroup(answers)

    // Then
    actual shouldBe expected
  }

  it should "Get the unanimous yes count per group" in {
    // Given
    val answers = Seq(
      CustomsAnswers(1, "abc"),
      CustomsAnswers(3, "abc"),
      CustomsAnswers(2, "abac"),
      CustomsAnswers(4, "aaaa"),
      CustomsAnswers(1, "b")
    )

    val expected = Seq(3, 0, 1, 1, 1)

    // When
    val actual = DaySix.getUnanimousYesCountPerGroup(answers)

    // Then
    actual shouldBe expected
  }

}
