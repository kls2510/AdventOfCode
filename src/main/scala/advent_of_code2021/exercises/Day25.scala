package advent_of_code2021.exercises

import util._

object Day25 extends WithLogger with Exercise[Vector[Vector[Char]]] {

    val dayNumber: Int = 25

    override def processRawLines(input: List[String]): Vector[Vector[Char]] = input.map(r => r.toCharArray().toVector).toVector

    def stepAcross(input: Vector[Vector[Char]]): Vector[Vector[Char]] = {
        (for (
            i <- 0 to input.length - 1
        ) yield {
            (for (
                j <- 0 to input(0).length - 1
            ) yield {
                val left = if (j > 0) (i, j - 1) else (i, input(0).length - 1)
                val right = if (j < input(0).length - 1) (i, j + 1) else (i, 0)
                input(i)(j) match {
                    case '.' => if (input(left._1)(left._2) == '>') '>' else '.'
                    case '>' => if (input(right._1)(right._2) == '.') '.' else '>'
                    case 'v' => 'v'
                }
            }).toVector
        }).toVector
    }

    def stepDown(input: Vector[Vector[Char]]): Vector[Vector[Char]] = {
        (for (
            i <- 0 to input.length - 1
        ) yield {
            (for (
                j <- 0 to input(0).length - 1
            ) yield {
                val up = if (i > 0) (i - 1, j) else (input.length - 1, j)
                val down = if (i < input.length - 1) (i + 1, j) else (0, j)
                input(i)(j) match {
                    case '.' => if (input(up._1)(up._2) == 'v') 'v' else '.'
                    case '>' => '>'
                    case 'v' => if (input(down._1)(down._2) == '.') '.' else 'v'
                }
            }).toVector
        }).toVector
    }

    def step(input: Vector[Vector[Char]]): Vector[Vector[Char]] = stepDown(stepAcross(input))

    def runUntilStationary(input: Vector[Vector[Char]]): (Vector[Vector[Char]], Int) = {
        def _run(state: Vector[Vector[Char]], iter: Int): (Vector[Vector[Char]], Int) = {
            val nextState = step(state)
            if (nextState == state) (state, iter) else _run(nextState, iter + 1)
        }
        _run(input, 1)
    }

    override def part1(input: Vector[Vector[Char]]): Unit = {
        logInfo("Starting part 1")
        val result = runUntilStationary(input)
        logInfo("Num steps = " + result._2)
    }

    override def part2(input: Vector[Vector[Char]]): Unit = {
        logInfo("Starting part 2")
        
    }
}
