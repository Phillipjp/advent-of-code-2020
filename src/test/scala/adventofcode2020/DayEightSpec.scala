package adventofcode2020

import adventofcode2020.DayEight.{Accumulate, Instruction, Jump, NoOperation}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DayEightSpec extends AnyFlatSpecLike with Matchers {

  it should "read in instructions" in {
    // Given
    val expected = Seq(
      Instruction(NoOperation, 0, false),
      Instruction(Accumulate, 1, false),
      Instruction(Jump, 4, false),
      Instruction(Accumulate, 3, false),
      Instruction(Jump, -3, false),
      Instruction(Accumulate, -99, false),
      Instruction(Accumulate, 1, false),
      Instruction(Jump, -4, false),
      Instruction(Accumulate, 6, false)
    )
    // When
    val actual = DayEight.readInstructions("day-8-part-1-test.txt")

    // Then
    actual shouldBe expected
  }

  it should "get the final value of the accumulator before the instructs repeat" in {
    // Given
    val instructions = Seq(
      Instruction(NoOperation, 0, false),
      Instruction(Accumulate, 1, false),
      Instruction(Jump, 4, false),
      Instruction(Accumulate, 3, false),
      Instruction(Jump, -3, false),
      Instruction(Accumulate, -99, false),
      Instruction(Accumulate, 1, false),
      Instruction(Jump, -4, false),
      Instruction(Accumulate, 6, false)
    )

    // When
    val actual = DayEight.findAccumulatorValue(instructions)

    // Then
    actual shouldBe 5
  }

  it should "get the broken route in the instructions" in {
    // Given
    val instructions = Seq(
      Instruction(NoOperation, 0, false),
      Instruction(Accumulate, 1, false),
      Instruction(Jump, 4, false),
      Instruction(Accumulate, 3, false),
      Instruction(Jump, -3, false),
      Instruction(Accumulate, -99, false),
      Instruction(Accumulate, 1, false),
      Instruction(Jump, -4, false),
      Instruction(Accumulate, 6, false)
    )

    val expected = Seq(0, 1, 2, 6, 7, 3, 4, 1)

    // When
    val actual = DayEight.getBrokenRoute(instructions)

    // Then
    actual shouldBe expected
  }

  it should "fix the instructions" in {
    // Given
    val instructions = Seq(
      Instruction(NoOperation, 0, false),
      Instruction(Accumulate, 1, false),
      Instruction(Jump, 4, false),
      Instruction(Accumulate, 3, false),
      Instruction(Jump, -3, false),
      Instruction(Accumulate, -99, false),
      Instruction(Accumulate, 1, false),
      Instruction(Jump, -4, false),
      Instruction(Accumulate, 6, false)
    )

    val brokenRoute = Seq(0, 1, 2, 6, 7, 3, 4, 1)

    val expected = Seq(
      Instruction(NoOperation, 0, false),
      Instruction(Accumulate, 1, false),
      Instruction(Jump, 4, false),
      Instruction(Accumulate, 3, false),
      Instruction(Jump, -3, false),
      Instruction(Accumulate, -99, false),
      Instruction(Accumulate, 1, false),
      Instruction(NoOperation, -4, false),
      Instruction(Accumulate, 6, false)
    )

    // When
    val actual = DayEight.fixInstructions(instructions, brokenRoute)

    // Then
    actual shouldBe expected
  }
}
