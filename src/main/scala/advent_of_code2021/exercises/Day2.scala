package advent_of_code2021.exercises

import scala.io.Source

import util._


case class Vec2(x: Int, y: Int) {
  def +(that: Vec2): Vec2 = Vec2(x + that.x, y + that.y)
  def mul(): Int = x * y
}

case class Vec3(x: Int = 0, y: Int = 0, aim: Int = 0) {
  def +(that: Vec2): Vec3 = that match {
    case Vec2(0, i)=> Vec3(x, y, aim + i)
    case Vec2(i, 0) => Vec3(x + i, y + aim * i, aim)
  }
  def mul(): Int = x * y
}

object Day2 extends WithLogger with Exercise[List[Vec2]] {

  override val dayNumber: Int = 2

  override def processRawLines(input: List[String]): List[Vec2] = {
    input.map(line => {
      val spl = line.split(" ").toList
      spl match {
        case "up" :: amt :: _ => Vec2(0, -amt.toInt)
        case "down" :: amt :: _ => Vec2(0, amt.toInt)
        case "forward" :: amt :: _ => Vec2(amt.toInt, 0)
      } 
    })
  }

  override def part1(input: List[Vec2]): Unit = {
    val finalDir = input.reduce(_ + _)
    logInfo("Final Direction = " + finalDir.toString)
    logInfo("Answer = " + finalDir.mul.toString)
  }

  override def part2(input: List[Vec2]): Unit = {
    val finalDir = input.foldLeft(Vec3()){
      (acc, dir) => acc + dir
    }
    logInfo("Final Direction = " + finalDir.toString)
    logInfo("Answer = " + finalDir.mul.toString)
  }

}
