package advent_of_code2021.exercises

import util._


object Day7 extends WithLogger with Exercise[List[Int]] {

    val dayNumber: Int = 7

    override def processRawLines(input: List[String]): List[Int] = {
        input match {
            case s :: Nil => s.split(",").map(i => i.toInt).toList
        }
    }

    override def part1(input: List[Int]): Unit = {
        logInfo("Starting part 1")
        
        val target = input.sorted.take(input.length / 2).reverse.head
        logInfo("Min movement " + input.map(i => math.abs(i - target)).reduce(_ + _))
    }

    override def part2(input: List[Int]): Unit = {
        logInfo("Starting part 2")
        
        val mean = input.sum / input.length.toFloat
        val targets = List(math.floor(mean), math.ceil(mean))
        val minDiff = targets.map(t => input.map(i => {
            val n = math.abs(i - t.toInt)
            (n * (n + 1)) / 2
        }).reduce(_ + _)).min
        logInfo("Min movement " + minDiff)
    }
}
