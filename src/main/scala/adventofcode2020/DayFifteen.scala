package adventofcode2020

object DayFifteen {

  def playMemoryGame(staringNumbers: Seq[Int], n: Int): Int = {

    val previousTurns = staringNumbers.zipWithIndex.map { case (n, i) => n -> (i + 1, 0) }.toMap
    val lastSpokenNumber = staringNumbers.last
    val currentTurn = staringNumbers.length + 1

    def play(previousTurns: Map[Int, (Int, Int)], lastSpokenNumber: Int, currentTurn: Int): Int = {
      if (currentTurn > n) {
        lastSpokenNumber
      }
      else {
        if (previousTurns(lastSpokenNumber)._2 == 0) {
          val newNumber = 0
          if(previousTurns.keySet.contains(newNumber)) {
            val updatedPreviousTurns = previousTurns + (newNumber -> (currentTurn, previousTurns(newNumber)._1))
            play(updatedPreviousTurns, newNumber, currentTurn + 1)
          }
          else{
            val updatedPreviousTurns = previousTurns + (newNumber -> (currentTurn, 0))
            play(updatedPreviousTurns, newNumber, currentTurn + 1)
          }
        }
        else {
          val newNumber = previousTurns(lastSpokenNumber)._1 - previousTurns(lastSpokenNumber)._2
          if (previousTurns.keySet.contains(newNumber)) {
            val updatedPreviousTurns = previousTurns + (newNumber -> (currentTurn, previousTurns(newNumber)._1))
            play(updatedPreviousTurns, newNumber, currentTurn + 1)
          }
          else {
            val updatedPreviousTurns = previousTurns + (newNumber -> (currentTurn, 0))
            play(updatedPreviousTurns, newNumber, currentTurn + 1)
          }
        }
      }
    }

    play(previousTurns, lastSpokenNumber, currentTurn)

  }

  def main(args: Array[String]): Unit = {
    val partOne = playMemoryGame(Seq(1,20,8,12,0,14), 2020)
    println(s"Part One: $partOne")

    val partTwo = playMemoryGame(Seq(1,20,8,12,0,14), 30000000)
    println(s"Part One: $partTwo")
  }

}
