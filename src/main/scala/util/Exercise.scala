package util

import scala.io.Source


trait Exercise[B] {
    val dayNumber: Int

    def main(args: Array[String]): Unit = {
        val rawInputLines = Control.using(
            Source.fromFile(
                getClass.getResource("/2021/day" + dayNumber.toString + ".txt").getFile()
            )
        ) { source =>
            (for (line <- source.getLines) yield line).toList
        }
        val input = processRawLines(rawInputLines)
        part1(input)
        part2(input)
    }

    def processRawLines(input: List[String]): B
    def part1(input: B): Unit
    def part2(input: B): Unit
}
