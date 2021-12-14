package advent_of_code2021.exercises

import util._


case class Paper(coords: Set[(Int, Int)]) extends WithLogger {
    def fold(instruction: Fold): Paper = {
        instruction match {
            case VerticalFold(x) => Paper(coords.filter(_._1 != x).map(
                origCoord => (math.abs(x - origCoord._1) - 1, origCoord._2)
            ))
            case HorizontalFold(y) => {
                val newc = coords.filter(_._2 != y).map(
                    origCoord => (origCoord._1, y - math.abs((origCoord._2 - y)))
                )
                val offset = newc.map(_._2).filter(_ < 0).minOption
                offset match {
                    case Some(m) => Paper(newc.map(c => (c._1, c._2 - m)))
                    case _ => Paper(newc)
                }
            }
        }
    }

    val numCoords = coords.size
    val maxX = coords.map(_._1).max
    val maxY = coords.map(_._2).max

    def print(): Unit = {
        for (
            j <- 0 to maxY
        ) {
            val row = List.range(0, maxX + 1).foldLeft("")(
                (r, c) => if (coords.contains((c, j))) r + "#" else r + " "
            )
            logInfo(row.reverse)
        }
    }
}


trait Fold
case class HorizontalFold(y: Int) extends Fold
case class VerticalFold(x: Int) extends Fold

object Day13 extends WithLogger with Exercise[(Paper, List[Fold]) ] {

    val dayNumber: Int = 13

    override def processRawLines(input: List[String]): (Paper, List[Fold]) = {
        val reading = input.foldLeft((Set(): Set[(Int, Int)], List(): List[Fold])){
            (agg, lineInfo) => {
                if (lineInfo.startsWith("fold")) {
                    (agg._1, (if (lineInfo.contains("x")) VerticalFold(lineInfo.split("=")(1).toInt) else HorizontalFold(lineInfo.split("=")(1).toInt)) :: agg._2)
                } else if (lineInfo.contains(",")) {
                    val newCoords = lineInfo.split(",").take(2).map(_.toInt).toList match { case a :: b :: _ => (a, b)}
                    (agg._1 + newCoords, agg._2)
                } else agg
            }
        }
        (Paper(reading._1), reading._2.reverse)
    }


    override def part1(input: (Paper, List[Fold]) ): Unit = {
        logInfo("Starting part 1")
        val instructions = input._2.take(1)
        val newPaper = instructions.foldLeft(input._1)((paper, instruction) => paper.fold(instruction))
        logInfo("Dots visible = " + newPaper.numCoords.toString)
    }

    override def part2(input: (Paper, List[Fold]) ): Unit = {
        logInfo("Starting part 2")
        val newPaper = input._2.foldLeft(input._1)((paper, instruction) => paper.fold(instruction))
        newPaper.print
    }
}
