package advent_of_code2021.exercises

import scala.io.Source

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

    def markNonDiagonalsOnGrid(grid: List[List[Int]]): List[List[Int]] = {
        (start, end) match {
            case (Point(x1, y1), Point(x2, y2)) if x1 == x2 => {
                val colNo = x1
                List.range(minY, maxY + 1).foldLeft(grid)(
                    (newRows, y) => newRows.updated(y, newRows(y).updated(colNo, newRows(y)(colNo) + 1))
                )
            }
            case (Point(x1, y1), Point(x2, y2)) if y1 == y2 => {
                val rowNo = y1
                List.range(minX, maxX + 1).foldLeft(grid)(
                    (newRows, x) => newRows.updated(rowNo, newRows(rowNo).updated(x, newRows(rowNo)(x) + 1))
                )
            }
            case _ => grid
        }
    }

    def markAllOnGrid(grid: List[List[Int]]): List[List[Int]] = {
        (start, end) match {
            case (Point(x1, y1), Point(x2, y2)) if x1 != x2 && y1 != y2 => {
                val hor = x2 > x1
                val ver = y2 > y1
                val xList = List.range(minX, maxX + 1)
                val yList = List.range(minY, maxY + 1)
                val coords = (hor, ver) match {
                    case (true, true) => xList zip yList
                    case (true, false) => xList zip yList.reverse
                    case (false, true) => xList.reverse zip yList
                    case (false, false) => xList.reverse zip yList.reverse
                }
                coords.foldLeft(grid)(
                    (newRows, coord) => newRows.updated(coord._2, newRows(coord._2).updated(coord._1, newRows(coord._2)(coord._1) + 1))
                )
            }
            case _ => markNonDiagonalsOnGrid(grid)
        }
    }
}

object Day5 extends WithLogger with Exercise[(List[Line], Int, Int)] {

    val dayNumber: Int = 5

    override def processRawLines(input: List[String]): (List[Line], Int, Int) = {
        val lines = input.map(line => {
            val points = line.split("->").map(Point.fromString(_)).toList
            points match {
                case p1 :: p2 :: _ => Line(p1, p2)
            }
        })
        (lines, lines.map(_.maxX).max, lines.map(_.maxY).max)
    }

    def genGrid(width: Int, height: Int): List[List[Int]] = List.range(0, height + 1).map(
        _ => {
            List.range(0, width + 1).map(
                _ => 0
            )
        }
    )

    override def part1(input: (List[Line], Int, Int)): Unit = {
        logInfo("Starting part 1")
        
        val grid = genGrid(input._2, input._3)
        val finalGrid = input._1.foldLeft(grid)(
            (grid, line) => {
                line.markNonDiagonalsOnGrid(grid)
            }
        )
        logInfo("Score = " + finalGrid.flatten.filter(_ > 1).map(_ => 1).reduce(_ + _).toString)
    }

    override def part2(input: (List[Line], Int, Int)): Unit = {
        logInfo("Starting part 2")

        val grid = genGrid(input._2, input._3)
        val finalGrid = input._1.foldLeft(grid)(
            (grid, line) => {
                line.markAllOnGrid(grid)
            }
        )
        logInfo("Score = " + finalGrid.flatten.filter(_ > 1).map(_ => 1).reduce(_ + _).toString)
    }
}
