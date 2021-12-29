package advent_of_code2021.exercises

import util._

object Cuboid {
    def fromInput(input: String): Cuboid = {
        val spl1 = input.split(" ")
        val onOff = spl1(0) == "on"
        val coordsList = spl1(1).split(",")
        val coords = coordsList.map(s => s.split("=")(1).split("\\.\\.").map(Integer.parseInt(_)))
        val ftl = Vec3(coords(0)(0), coords(1)(0), coords(2)(1))
        val fbr = Vec3(coords(0)(1), coords(1)(0), coords(2)(0))
        val bbr = Vec3(coords(0)(1), coords(1)(1), coords(2)(0))
        Cuboid(ftl, fbr, bbr, onOff)
    }
}

case class Cuboid(ftl: Vec3, fbr: Vec3, bbr: Vec3, on: Boolean) extends WithLogger {
    def getCubeList(): Iterator[Vec3] = {
        (for (
            x <- (ftl.x to fbr.x).iterator;
            y <- (fbr.y to bbr.y).iterator;
            z <- (fbr.z to ftl.z).iterator
         ) yield {
            Vec3(x, y, z)
        })
    }

    def cubeLiesInside(cube: Vec3): Boolean = {
        cube.x >= ftl.x && cube.x <= fbr.x && cube.y >= fbr.y && cube.y <= bbr.y && cube.z <= ftl.z && cube.z >= fbr.z
    }

    def size(): Long = (fbr.x - ftl.x + 1L) * (bbr.y - fbr.y + 1L) * (ftl.z - fbr.z + 1L)

    def findOverlap(other: Cuboid): Option[Cuboid] = {
        val maybeXes = (ftl.x, fbr.x, other.ftl.x, other.fbr.x) match {
            case (x1, _, _, ox2) if ox2 < x1 => None
            case (_, x2, ox1, _) if ox1 > x2 => None
            case (x1, x2, ox1, ox2) => Some((List(x1, ox1).max, List(x2, ox2).min))
        }
        val maybeYes = (fbr.y, bbr.y, other.fbr.y, other.bbr.y) match {
            case (y1, _, _, oy2) if oy2 < y1 => None
            case (_, y2, oy1, _) if oy1 > y2 => None
            case (y1, y2, oy1, oy2) => Some((List(y1, oy1).max, List(y2, oy2).min))
        }
        val maybeZes = (fbr.z, ftl.z, other.fbr.z, other.ftl.z) match {
            case (z1, _, _, oz2) if oz2 < z1 => None
            case (_, z2, oz1, _) if oz1 > z2 => None
            case (z1, z2, oz1, oz2) => Some((List(z1, oz1).max, List(z2, oz2).min))
        }
        (maybeXes, maybeYes, maybeZes) match {
            case (Some((x1, x2)), Some((y1, y2)), Some((z1, z2))) => Some(
                Cuboid(Vec3(x1, y1, z2), Vec3(x2, y1, z1), Vec3(x2, y2, z1), other.on)
            )
            case _ => None
        }
    }

    def subtract(other: Cuboid): List[Cuboid] = {
        val maybeLeftX = if (ftl.x < other.ftl.x) (ftl.x, other.ftl.x - 1) else (ftl.x, ftl.x)
        val maybeRightX = if (fbr.x > other.fbr.x) (other.fbr.x + 1, fbr.x) else (fbr.x, fbr.x)

        val maybeFrontY = if (fbr.y < other.fbr.y) (fbr.y, other.fbr.y - 1) else (fbr.y, fbr.y)
        val maybeBackY = if (bbr.y > other.bbr.y) (other.bbr.y + 1, bbr.y) else (bbr.y, bbr.y)

        val maybeTopZ = if (ftl.z > other.ftl.z) (other.ftl.z + 1, ftl.z) else (ftl.z, ftl.z)
        val maybeBottomZ = if (fbr.z < other.fbr.z) (fbr.z, other.fbr.z - 1) else (fbr.z, fbr.z)

        (maybeLeftX, maybeRightX, maybeFrontY, maybeBackY, maybeBottomZ, maybeTopZ) match {
            case ((x1, x2), (x3, x4), (y1, y2), (y3, y4), (z1, z2), (z3, z4)) => {
                val maybeBlock1 = if (ftl.x < other.ftl.x) Some(Cuboid(Vec3(x1, y1, z4), Vec3(x2, y1, z1), Vec3(x2, y4, z1), false)) else None
                val maybeBlock2 = if (fbr.x > other.fbr.x) Some(Cuboid(Vec3(x3, y1, z4), Vec3(x4, y1, z1), Vec3(x4, y4, z1), false)) else None

                val leftOffset = if (maybeBlock1.isDefined) 1 else 0
                val rightOffset = if (maybeBlock2.isDefined) 1 else 0
                val maybeBlock3 = if (fbr.y < other.fbr.y) Some(Cuboid(Vec3(x2 + leftOffset, y1, z4), Vec3(x3 - rightOffset, y1, z1), Vec3(x3 - rightOffset, y2, z1), false)) else None
                val maybeBlock4 = if (bbr.y > other.bbr.y) Some(Cuboid(Vec3(x2 + leftOffset, y3, z4), Vec3(x3 - rightOffset, y3, z1), Vec3(x3 - rightOffset, y4, z1), false)) else None

                val frontOffset = if (maybeBlock3.isDefined) 1 else 0
                val backOffset = if (maybeBlock4.isDefined) 1 else 0
                val maybeBlock5 = if (fbr.z < other.fbr.z) Some(Cuboid(Vec3(x2 + leftOffset, y2 + frontOffset, z2), Vec3(x3 - rightOffset, y2 + frontOffset, z1), Vec3(x3 - rightOffset, y3 - backOffset, z1), false)) else None
                val maybeBlock6 = if (ftl.z > other.ftl.z) Some(Cuboid(Vec3(x2 + leftOffset, y2 + frontOffset, z4), Vec3(x3 - rightOffset, y2 + frontOffset, z3), Vec3(x3 - rightOffset, y3 - backOffset, z3), false)) else None
                List(maybeBlock1, maybeBlock2, maybeBlock3, maybeBlock4, maybeBlock5, maybeBlock6).filter(_.isDefined).map(_.get)
            }
        }
    }

    def splitOtherCuboidOnAdd(other: Cuboid): List[Cuboid] = {
        val maybeOverlap = findOverlap(other)
        maybeOverlap match {
            case Some(overlap) => other.subtract(overlap)
            case None => List(other)
        }
    }

    def removeOtherCuboid(other: Cuboid): List[Cuboid] = {
        val maybeOverlap = findOverlap(other)
        maybeOverlap match {
            case Some(overlap) => this.subtract(overlap)
            case None => List(this)
        }
    }
    
}

case class Grid3D(cuboids: List[Cuboid]) extends WithLogger {

    // Don't run on large areas e.g part 2
    def countIn(area: Cuboid): Long = {
        val toCheck = area.getCubeList
        logInfo("Counting")
        toCheck.filter(
            cube => {
                cuboids.foldLeft(false)(
                    (state, cuboid) => if (cuboid.cubeLiesInside(cube)) cuboid.on else state
                )
            }
        ).length
    }

    def _reduceCuboidOnAdd(cuboids: List[Cuboid], toAdd: Cuboid): List[Cuboid] = {
        cuboids.foldLeft(List(toAdd): List[Cuboid])(
            (allSplits, next) => allSplits.map(next.splitOtherCuboidOnAdd(_)).flatten
        )
    }

    def countAll(): Long = {
        val finalCuboids = cuboids.foldLeft(List(): List[Cuboid])(
            (allCuboids, next) => {
                if (next.on) {
                    val additions = _reduceCuboidOnAdd(allCuboids, next)
                    allCuboids ++ additions
                } else allCuboids.map(_.removeOtherCuboid(next)).flatten
            }
        )
        finalCuboids.map(_.size).reduce(_ + _)
    }
}

object Day22 extends WithLogger with Exercise[List[Cuboid]] {

    val dayNumber: Int = 22

    override def processRawLines(input: List[String]): List[Cuboid] = input.map(Cuboid.fromInput(_))

    override def part1(input: List[Cuboid]): Unit = {
        logInfo("Starting part 1")
        val grid3d = Grid3D(input)
        val numberOn = grid3d.countIn(Cuboid(Vec3(-50, -50, 50), Vec3(50, -50, -50), Vec3(50, 50, -50), true))
        logInfo("Number cubes on = " + numberOn)
    }

    override def part2(input: List[Cuboid]): Unit = {
        logInfo("Starting part 2")
        val grid3d = Grid3D(input)
        val numberOn = grid3d.countAll()
        logInfo("Number cubes on = " + numberOn)
    }
}
