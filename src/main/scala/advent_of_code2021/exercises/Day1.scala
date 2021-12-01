package advent_of_code2021.exercises

import scala.io.Source

import util._


object Day1 extends WithLogger with Exercise[List[Int]] {

    val dayNumber: Int = 1

    override def processRawLines(input: List[String]): List[Int] = input.map(_.toInt)

    def findIncreases(firstValList: List[Int], secondValList: List[Int]): Unit = {
        val depthIncreases = (firstValList zip secondValList).foldLeft(0)({
            (acc, depths) => acc + (if (depths._1 < depths._2) 1 else 0)
        })

        logInfo("Depth increases: " + depthIncreases.toString)
    }

    def applyWindow(allVal: List[Int], window: Int): Unit = {
        val shifted = allVal.drop(window)
        findIncreases(allVal, shifted)
    }

    override def part1(allVal: List[Int]): Unit = {
        logInfo("Starting part 1")
        applyWindow(allVal, 1)
    }

    override def part2(allVal: List[Int]): Unit = {
        logInfo("Starting part 2")
        applyWindow(allVal, 3)
    }
}
