package advent_of_code2021.exercises

import util._

case class Target(tl: Vec2, br: Vec2) {
    def isInTarget(point: Vec2): Boolean = {
        point.x >= tl.x && point.x <= br.x && point.y >= br.y && point.y <= tl.y
    }

    def isOvershot(point: Vec2): Boolean = {
        point.x > br.x || point.y < br.y
    }
}


object Day17 extends WithLogger with Exercise[Target] {

    val dayNumber: Int = 17

    def extractCoords(input: String): List[Int] = {
        val cmp = input.split("=")(1).split("\\.\\.").toList
        cmp match {
            case v1 :: v2 :: _ => List(v1.toInt, v2.toInt)
        }
    }

    override def processRawLines(input: List[String]): Target = {
        val line = input(0)
        val numbers = line.split(":")(1).strip()
        val xAndY = numbers.split(", ").toList
        xAndY match {
            case xStr :: yStr:: _ => {
                val xs = extractCoords(xStr)
                val ys = extractCoords(yStr)
                val tl = Vec2(xs.min, ys.max)
                val br = Vec2(xs.max, ys.min)
                Target(tl, br)
            }
        }
    }

    def sumFirstN(n: Int): Int = (n * (n + 1)) / 2

    def step(position: Vec2, velocity: Vec2): (Vec2, Vec2) = (position + velocity, Vec2(List(velocity.x - 1, 0).max, velocity.y - 1))

    def simulate(initialV: Vec2, target: Target): Boolean = {
        def _sim(pos: Vec2, vel: Vec2): Boolean = {
            if (target.isInTarget(pos)) true
            else {
                if (target.isOvershot(pos)) false
                else {
                    val next = step(pos, vel)
                    _sim(next._1, next._2)
                }
            }
        }
        _sim(Vec2(0, 0), initialV)
    }

    def findMinX(target: Target): Int = {
        val lt = List.range(1, 100).takeWhile(n => sumFirstN(n) < target.tl.x)
        lt.length + 1
    }

    override def part1(input: Target): Unit = {
        logInfo("Starting part 1")

        val yStarts = List.range(1, 500).reverse
        val xStarts = List.range(findMinX(input), input.br.x + 1)
        val validY = yStarts.find(
            yStart => {
                xStarts.find(xStart => simulate(Vec2(xStart, yStart), input)).isDefined
            }
        )
        logInfo("Best height = " + sumFirstN(validY.get))
    }

    override def part2(input: Target): Unit = {
        logInfo("Starting part 2")
        
        val yStarts = List.range(-500, 500).reverse
        val xStarts = List.range(findMinX(input), input.br.x + 1)
        val allSolutions = yStarts.foldLeft(List(): List[Vec2])(
            (allSolutions, yStart) => {
                val valids = xStarts.map(xStart => {
                    val v = Vec2(xStart, yStart)
                    if (simulate(Vec2(xStart, yStart), input)) Some(v) else None
                }).filter(_.isDefined).map(_.get)
                allSolutions ++ valids
            }
        )
        logInfo("Number of solutions = " + allSolutions.length)
    }
}
