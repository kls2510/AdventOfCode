package advent_of_code2021.exercises

import util._


object Day9 extends WithLogger with Exercise[Vector[Vector[Int]]] {

    val dayNumber: Int = 9

    override def processRawLines(input: List[String]): Vector[Vector[Int]] = {
        input.foldLeft(Vector(): Vector[Vector[Int]])(
            (allLines, line) => allLines :+ line.toCharArray.toVector.map(c => Integer.parseInt(c.toString))
        )
    }

    def getNeighbours(coords: (Int, Int), width: Int, height: Int): List[(Int, Int)] = {
        val row = coords._1
        val col = coords._2
        val up = if (row > 0) Some((row - 1, col)) else None
        val down = if (row + 1 < height) Some((row + 1, col)) else None
        val left = if (col > 0) Some((row, col - 1)) else None
        val right = if (col + 1 < width) Some((row, col + 1)) else None
        List(up, down, left, right).filter(_.isDefined).map(_.get)
    }

    def getLowPoints(input: Vector[Vector[Int]]): Map[(Int, Int), Int] = {
        (for (
            row <- 0 to input.length - 1;
            col <- 0 to input.head.length - 1
        ) yield {
            val current = input(row)(col)
            val neighbours = getNeighbours((row, col), input.head.length, input.length).map(c => input(c._1)(c._2))
            val lowerThanAll = neighbours.map(v => current < v).forall(identity)
            if (lowerThanAll) Some((row, col) -> current) else None
        }).filter(_.isDefined).map(_.get).toMap
    }

    override def part1(input: Vector[Vector[Int]]): Unit = {
        logInfo("Starting part 1")
        
        val lowPoints = getLowPoints(input)
        logInfo("Risk Level = " + lowPoints.map(1 + _._2).reduce(_ + _).toString)
    }

    def DFS(start: (Int, Int), visiteds: Set[(Int, Int)], grid: Vector[Vector[Int]]): Set[(Int, Int)] = {
        if (visiteds.contains(start)) visiteds else {
            val neighbours = getNeighbours(start, grid.head.length, grid.length).filter(coord => !visiteds.contains(coord) && grid(coord._1)(coord._2) != 9)
            neighbours.foldLeft(visiteds + start)((currentVisiteds, newStart) => DFS(newStart, currentVisiteds, grid))
        }
    }

    override def part2(input: Vector[Vector[Int]]): Unit = {
        logInfo("Starting part 2")

        val lowPointsVisited = getLowPoints(input).map(m => m._1 -> false)
        val lowPointLocs = lowPointsVisited.keySet

        val search = lowPointLocs.toList.foldLeft((List(): List[Set[(Int, Int)]], lowPointsVisited)){
            case ((allBasins, visitedLowPoints), currentLowPoint) => {
                if (visitedLowPoints(currentLowPoint)) (allBasins, visitedLowPoints) else {
                    val basin = DFS(currentLowPoint, Set(), input)
                    val lowPointsRemoved = visitedLowPoints.map(m => if (basin.contains(m._1)) m._1 -> true else m._1 -> false)
                    (basin :: allBasins, lowPointsRemoved)
                }
            }
        }
        val allBasins = search._1
        val basinSizes = allBasins.map(_.size)
        logInfo("Result = " + basinSizes.sorted.reverse.take(3).reduce(_ * _))
    }
}
