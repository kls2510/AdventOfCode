package advent_of_code2021.exercises

import util._


object Day11 extends WithLogger with Exercise[Vector[Vector[Int]]] {

    val dayNumber: Int = 11

    override def processRawLines(input: List[String]): Vector[Vector[Int]] = {
        input.foldLeft(Vector(): Vector[Vector[Int]])(
            (allLines, line) => allLines :+ line.toCharArray.toVector.map(c => Integer.parseInt(c.toString))
        )
    }

    def addOne(input: Vector[Vector[Int]]): Vector[Vector[Int]] = input.map(row => row.map(v => v + 1))

    def getNeighbours(coords: (Int, Int), width: Int, height: Int): List[(Int, Int)] = {
        val allNeighbours = (for (
            i <- -1 to 1;
            j <- -1 to 1
        ) yield (coords._1 + i, coords._2 + j)).toList
        allNeighbours.filter(c => c._1 >= 0 && c._1 < height && c._2 >= 0 && c._2 < width && c != coords)
    }

    def getAllToFlash(input: Vector[Vector[Int]]): List[(Int, Int)] = {
        (for (
            i <- 0 to input.length - 1;
            j <- 0 to input.length - 1
        ) yield (i, j)).filter(c => input(c._1)(c._2) > 9).toList
    }

    def flash(coord: (Int, Int), input: Vector[Vector[Int]]): Vector[Vector[Int]] = {
        val thisFlashed = input.updated(coord._1, input(coord._1).updated(coord._2, -1))
        val neighbours = getNeighbours(coord, input(0).length, input.length)
        val notFlashedNeighbours = neighbours.filter(c => input(c._1)(c._2) != -1)
        notFlashedNeighbours.foldLeft(thisFlashed)(
            (nextState, neighbour) => nextState.updated(neighbour._1, nextState(neighbour._1).updated(neighbour._2, nextState(neighbour._1)(neighbour._2) + 1))
        )
    }

    def countFlashed(input: Vector[Vector[Int]]): Int = input.flatten.filter(_ == -1).length

    def resetFlashed(input: Vector[Vector[Int]]): Vector[Vector[Int]] = input.map(row => row.map(v => if (v == -1) 0 else v))

    def flashUntilNoneLeft(input: Vector[Vector[Int]]): Vector[Vector[Int]] = getAllToFlash(input) match {
       case c :: _ => flashUntilNoneLeft(flash(c, input))
       case Nil => input
    }

    def step(input: Vector[Vector[Int]]): (Vector[Vector[Int]], Int) = {
        val oneAdded = addOne(input)
        val flashes = flashUntilNoneLeft(oneAdded)
        (resetFlashed(flashes), countFlashed(flashes))
    }

    override def part1(input: Vector[Vector[Int]]): Unit = {
        logInfo("Starting part 1")

        val sim = List.range(0, 100).foldLeft((0, input)){ case ((numFlashes, state), t) => {
            val next = step(state)
            (numFlashes + next._2, next._1)
        }}

        logInfo("Num flashes = " + sim._1.toString)
    }

    def stepUntilAllFlashed(input: Vector[Vector[Int]]): Int = {
        if (input.flatten.map(_ == 0).forall(identity)) 0 else {
            1 + stepUntilAllFlashed(step(input)._1)
        }
    }

    override def part2(input: Vector[Vector[Int]]): Unit = {
        logInfo("Starting part 2")

        val numSteps = stepUntilAllFlashed(input)
        logInfo("Number of steps to all flash: " + numSteps.toString)
    }
}
