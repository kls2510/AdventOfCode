package advent_of_code2021.exercises

import util._


object Day21 extends WithLogger with Exercise[(Int, Int)] {

    val dayNumber: Int = 21

    def getStartingPos(line: String): Int = Integer.parseInt(line.split(":")(1).trim())

    override def processRawLines(input: List[String]): (Int, Int) = (getStartingPos(input(0)), getStartingPos(input(1)))

    def deterministicDiceRolls(player: Int, turn: Int): Int = {
        val base = if (player == 1) 1 else 4
        val unnormalR1 = (base + 6 * (turn - 1)) % 100
        val r1 = if (unnormalR1 == 0) 100 else unnormalR1
        3 * r1 + 3
    }

    def playGame(start: (Int, Int)): (Int, Int) = {
        def _play(turn: Int, p1Pos: Int, p2Pos: Int, p1Score: Int, p2Score: Int): (Int, Int) = {
            val p1MoveUnnormal = (p1Pos + deterministicDiceRolls(1, turn)) % 10
            val newP1Pos = if (p1MoveUnnormal == 0) 10 else p1MoveUnnormal
            val newP1Score = p1Score + newP1Pos
            if (newP1Score >= 1000) return (p2Score, (turn - 1) * 6 + 3)
            val p2MoveUnnormal = (p2Pos + deterministicDiceRolls(2, turn)) % 10
            val newP2Pos = if (p2MoveUnnormal == 0) 10 else p2MoveUnnormal
            val newP2Score = p2Score + newP2Pos
            if (newP2Score >= 1000) return (p1Score, turn * 6)
            _play(turn + 1, newP1Pos, newP2Pos, newP1Score, newP2Score)
        }
        _play(1, start._1, start._2, 0, 0)
    }

    override def part1(input: (Int, Int)): Unit = {
        logInfo("Starting part 1")
        val losingScoreAndNumTurns = playGame(input)
        logInfo(losingScoreAndNumTurns._1 + " " + losingScoreAndNumTurns._2)
        logInfo("Result = " + losingScoreAndNumTurns._1 * losingScoreAndNumTurns._2)
    }

    def playRealGame(start: (Int, Int), possibleRolls: List[List[Int]]): (Long, Long) = {
        val allRolls = possibleRolls.map(diceRolls => diceRolls.reduce(_ + _))
        val allSumsToCount = allRolls.groupMapReduce(identity)(_ => 1)(_ + _)
        def _play(turn: Int, p1Pos: Int, p2Pos: Int, p1Score: Int, p2Score: Int): (Long, Long) = {
            // logInfo(turn + " : " + p1Score + ", " + p2Score)
            if (p1Score >= 21) (1L, 0L)
            else if (p2Score >= 21) (0L, 1L)
            else {
                val player = turn % 2
                if (player == 1) {
                    allSumsToCount.foldLeft((0L, 0L))(
                        (outcomes, rollToCount) => {
                            val roll = rollToCount._1
                            val p1MoveUnnormal = (p1Pos + roll) % 10
                            val newP1Pos = if (p1MoveUnnormal == 0) 10 else p1MoveUnnormal
                            val newP1Score = p1Score + newP1Pos
                            val outcome = _play(turn + 1, newP1Pos, p2Pos, newP1Score, p2Score)
                            (outcomes._1 + (outcome._1 * rollToCount._2), outcomes._2 + (outcome._2 * rollToCount._2))
                        }
                    )
                } else {
                    allSumsToCount.foldLeft((0L, 0L))(
                        (outcomes, rollToCount) => {
                            val roll = rollToCount._1
                            val p2MoveUnnormal = (p2Pos + roll) % 10
                            val newP2Pos = if (p2MoveUnnormal == 0) 10 else p2MoveUnnormal
                            val newP2Score = p2Score + newP2Pos
                            val outcome = _play(turn + 1, p1Pos, newP2Pos, p1Score, newP2Score)
                            (outcomes._1 + (outcome._1 * rollToCount._2), outcomes._2 + (outcome._2 * rollToCount._2))
                        }
                    )
                }
            }
        }
        _play(1, start._1, start._2, 0, 0)
    }

    override def part2(input: (Int, Int)): Unit = {
        logInfo("Starting part 2")
        val l = List(List(1,2,3), List(1,1,2),  List(1,1,3), List(2,2,1), List(2,2,3), List(3,3,1), List(3,3,2), List(1,1,1), List(2,2,2), List(3,3,3))
        val allRolls = l.map(_.permutations.toList).flatten.toList
        val winCounts = playRealGame(input, allRolls)
        logInfo("Win counts: " + winCounts.toString)
        logInfo("Most wins: " + (if (winCounts._1 > winCounts._2) winCounts._1 else winCounts._2))
    }
}
