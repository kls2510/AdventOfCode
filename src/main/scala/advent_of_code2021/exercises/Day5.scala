package advent_of_code2021.exercises

import util._


case class Point(x: Int, y: Int)

object Point {
    def fromString(in: String): Point = {
        val numbers = in.split(",").toList
        numbers match {
            case i1 :: i2 :: _ => Point(i1.trim.toInt, i2.trim.toInt)
        }
    }
}

case class Line(start: Point, end: Point) extends WithLogger{
    val maxX: Int = List(start.x, end.x).max
    val maxY: Int = List(start.y, end.y).max
    val minX: Int = List(start.x, end.x).min
    val minY: Int = List(start.y, end.y).min
    val xList = List.range(minX, maxX + 1)
    val yList = List.range(minY, maxY + 1)
    val hor = end.x > start.x
    val ver = end.y > start.y
    val xyList = (hor, ver) match {
        case (true, true) => xList zip yList
        case (true, false) => xList zip yList.reverse
        case (false, true) => xList.reverse zip yList
        case (false, false) => xList.reverse zip yList.reverse
    }

    def markNonDiagonalsOnGrid(grid: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
        (start, end) match {
            case (Point(x1, y1), Point(x2, y2)) if x1 == x2 => {
                val colNo = x1
                yList.foldLeft(grid)(
                    (newGrid, y) => newGrid + ((colNo, y) -> (newGrid.getOrElse((colNo, y), 0) + 1))
                )
            }
            case (Point(x1, y1), Point(x2, y2)) if y1 == y2 => {
                val rowNo = y1
                xList.foldLeft(grid)(
                    (newGrid, x) => newGrid + ((x, rowNo) -> (newGrid.getOrElse((x, rowNo), 0) + 1))
                )
            }
            case _ => grid
        }
    }

    def markAllOnGrid(grid: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
        (start, end) match {
            case (Point(x1, y1), Point(x2, y2)) if x1 != x2 && y1 != y2 => {
                xyList.foldLeft(grid)(
                    (newGrid, coord) => newGrid + (coord -> (newGrid.getOrElse(coord, 0) + 1)) 
                )
            }
            case _ => markNonDiagonalsOnGrid(grid)
        }
    }
}

object Day5 extends WithLogger with Exercise[List[Line]] {

    val dayNumber: Int = 5

    override def processRawLines(input: List[String]): List[Line] = {
        input.map(line => {
            val points = line.split("->").map(Point.fromString(_)).toList
            points match {
                case p1 :: p2 :: _ => Line(p1, p2)
            }
        })
    }

    override def part1(input: List[Line]): Unit = {
        logInfo("Starting part 1")
        
        val grid: Map[(Int, Int), Int] = Map()
        val finalGrid = input.foldLeft(grid)(
            (grid, line) => {
                line.markNonDiagonalsOnGrid(grid)
            }
        )
        logInfo("Score = " + finalGrid.values.filter(_ > 1).map(_ => 1).reduce(_ + _).toString)
    }

    override def part2(input: List[Line]): Unit = {
        logInfo("Starting part 2")

        val grid: Map[(Int, Int), Int] = Map()
        val finalGrid = input.foldLeft(grid)(
            (grid, line) => {
                line.markAllOnGrid(grid)
            }
        )
        logInfo("Score = " + finalGrid.values.filter(_ > 1).map(_ => 1).reduce(_ + _).toString)
    }
}
