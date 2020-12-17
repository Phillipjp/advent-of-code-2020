package adventofcode2020

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DayFourteenSpec extends AnyFlatSpecLike with Matchers{

  it should "turn an integer into a 36 bit binary string" in {
    // Given, When, Then
    DayFourteen.intToBinaryString(101, 36) shouldBe "000000000000000000000000000001100101"
  }

  it should "convert a binary string to long" in {
    // Given, When, Then
    DayFourteen.binaryStringToLong("000000000000000000000000000001100101") shouldBe 101L
  }

  it should "run the program to completion and find the sume of the values in memory using decoder 1" in {
    // Given
    val program = Seq(
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0",
    )

    // When
    val actual = DayFourteen.runProgramWithDecoderOne(program)

    // Then
    actual shouldBe 165
  }

  it should "apply the mask to binary input and get all possible memory addresses" in {
    // Given
    val expected = Seq(
      "000000000000000000000000000000011010",
      "000000000000000000000000000000011011",
      "000000000000000000000000000000111010",
      "000000000000000000000000000000111011"
    )
    val binary = "000000000000000000000000000000101010"
    val mask = "000000000000000000000000000000X1001X"

    // When
    val actual = DayFourteen.getMemoryAddresses(binary, mask)

    // Then
    actual should contain theSameElementsAs  expected
  }

  it should "run the program to completion and find the sum of the values in memory using decoder 2" in {
    // Given
    val program = Seq(
      "mask = 000000000000000000000000000000X1001X",
      "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX",
      "mem[26] = 1"
    )

    // When
    val actual = DayFourteen.runProgramWithDecoderTwo(program)

    // Then
    actual shouldBe 208
  }
}
