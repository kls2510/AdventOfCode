package advent_of_code2021.exercises

import util._


object Day20 extends WithLogger with Exercise[(Map[Int, Char], Vector[Vector[Char]])] {

    val dayNumber: Int = 20

    override def processRawLines(input: List[String]): (Map[Int, Char], Vector[Vector[Char]]) = {
        val m = input(0).toCharArray.zipWithIndex.map{ case (c, i) => i -> c }.toMap
        val g = input.drop(2).map(row => row.toCharArray.toVector).toVector
        (m, g)
    }

    def char2Bin(c: Char): String = if (c == '.') "0" else "1"

    def pad(grid: Vector[Vector[Char]], amt: Int, char: Char): Vector[Vector[Char]] = {
        val newRows1 = for (i <- 0 to amt - 1) yield Vector.range(0, grid.length + (amt * 2)).map(_ => char).toVector
        val newRows2 = for (i <- 0 to amt - 1) yield Vector.range(0, grid.length + (amt * 2)).map(_ => char).toVector
        val newCols = grid.map(
            row => Vector.range(0, amt).map(_ => char) ++ row ++ Vector.range(0, amt).map(_ => char)
        )
        ((newRows1 ++: newCols) ++ newRows2).toVector
    }

    def enhance(grid: Vector[Vector[Char]], lookup: Map[Int, Char], numIter: Int): Vector[Vector[Char]] = {
        val fills = (lookup(0), lookup(lookup.keys.max)) match {
            case ('.', _) => (0, '.', '.')
            case ('#', '#') => (0, '.', '#')
            case ('#', '.') => (1, '.', '#')
        }
        def _enhance(input: Vector[Vector[Char]], step: Int): Vector[Vector[Char]] = {
            val padded = pad(input, 5, if (fills._1 == 0){
                if (step == 0) fills._2 else fills._3
            } else if (step % 2 == 0) fills._2 else fills._3)
            (for (
                i <- 3 to padded(0).length - 4
            ) yield {
                (for (
                    j <- 3 to padded.length - 4
                ) yield {
                    val allNeighbours = (for (
                        oi <- -1 to 1;
                        oj <- -1 to 1
                    ) yield (i + oi, j + oj)).toList
                    val lookupStr = allNeighbours.map{ case (x, y) => {
                        char2Bin(padded(x)(y))
                    } }.mkString
                    lookup(Integer.parseInt(lookupStr, 2))
                }).toVector
            }).toVector
        }
        List.range(0, numIter).foldLeft(grid)((currentState, step) => {
            logInfo("Step: " + step)
            _enhance(currentState, step)
        })
    }

    override def part1(input: (Map[Int, Char], Vector[Vector[Char]])): Unit = {
        logInfo("Starting part 1")
        val result = enhance(input._2, input._1, 2)
        logInfo("Number of lit pixels = " + result.map(row => row.filter(_ == '#')).flatten.length)
    }

    override def part2(input: (Map[Int, Char], Vector[Vector[Char]])): Unit = {
        logInfo("Starting part 2")
        val result = enhance(input._2, input._1, 50)
        logInfo("Number of lit pixels = " + result.map(row => row.filter(_ == '#')).flatten.length)
    }
}
