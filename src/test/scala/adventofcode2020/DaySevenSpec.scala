package adventofcode2020

import adventofcode2020.DaySeven.InnerBag
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DaySevenSpec extends AnyFlatSpecLike with Matchers{

  it should "get baggage rules" in {
    // Given
    val expected = Map(
      "light red bag" ->  Seq(InnerBag("bright white bag", 1), InnerBag("muted yellow bag", 2)),
      "dark orange bag" -> Seq(InnerBag("bright white bag", 3), InnerBag("muted yellow bag",4)),
      "bright white bag" -> Seq(InnerBag("shiny gold bag", 1)),
      "muted yellow bag" -> Seq(InnerBag("shiny gold bag", 2), InnerBag("faded blue bag", 9)),
      "shiny gold bag" -> Seq(InnerBag("dark olive bag", 1), InnerBag("vibrant plum bag", 2)),
      "dark olive bag" -> Seq(InnerBag("faded blue bag", 3), InnerBag("dotted black bag", 4)),
      "vibrant plum bag" -> Seq(InnerBag("faded blue bag", 5), InnerBag("dotted black bag", 6)),
      "faded blue bag" -> Seq(InnerBag("no other bags", 0)),
      "dotted black bag" -> Seq(InnerBag("no other bags", 0))
    )

    // When
    val actual = DaySeven.readBaggageRules("day-7-part-1-test.txt")

    // Then
    actual shouldBe expected
  }

  it should "find all the bags that eventually hold a shiny gold bag" in {
    // Given
    val rules = Map(
      "light red bag" ->  Seq(InnerBag("bright white bag", 1), InnerBag("muted yellow bag", 2)),
      "dark orange bag" -> Seq(InnerBag("bright white bag", 3), InnerBag("muted yellow bag",4)),
      "bright white bag" -> Seq(InnerBag("shiny gold bag", 1)),
      "muted yellow bag" -> Seq(InnerBag("shiny gold bag", 2), InnerBag("faded blue bag", 9)),
      "shiny gold bag" -> Seq(InnerBag("dark olive bag", 1), InnerBag("vibrant plum bag", 2)),
      "dark olive bag" -> Seq(InnerBag("faded blue bag", 3), InnerBag("dotted black bag", 4)),
      "vibrant plum bag" -> Seq(InnerBag("faded blue bag", 5), InnerBag("dotted black bag", 6)),
      "faded blue bag" -> Seq(InnerBag("no other bags", 0)),
      "dotted black bag" -> Seq(InnerBag("no other bags", 0))
    )

    val expected = Seq("muted yellow bag", "bright white bag", "light red bag", "dark orange bag")

    // When
    val actual = DaySeven.findWhatBagsContainGivenBag("shiny gold bag", rules.toSeq)

    // Then
    actual shouldBe expected

    actual.foreach(println)

  }

  it should "find the number of bags needed to fill a shiny gold bag" in {
    // Given
    val rules = DaySeven.readBaggageRules("day-7-part-2-test.txt")

    // When
    val actual = DaySeven.findNeededNumberOfBags("shiny gold bag", rules)

    println(actual)
  }

}
