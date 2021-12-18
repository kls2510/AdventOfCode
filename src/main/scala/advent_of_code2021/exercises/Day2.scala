package advent_of_code2021.exercises

import util._


case class D2Vec2(x: Int, y: Int) extends BaseVec2 {
  def mul(): Int = x * y
  def +(that: BaseVec2): D2Vec2 = D2Vec2(x + that.x, y + that.y)
}

case class D2Vec3(x: Int = 0, y: Int = 0, aim: Int = 0) extends BaseVec3 {
  val z = aim

  def +(that: D2Vec2): D2Vec3 = that match {
    case D2Vec2(0, i)=> new D2Vec3(x, y, aim + i)
    case D2Vec2(i, 0) => new D2Vec3(x + i, y + aim * i, aim)
  }
  def mul(): Int = x * y
}

object Day2 extends WithLogger with Exercise[List[D2Vec2]] {

  override val dayNumber: Int = 2

  override def processRawLines(input: List[String]): List[D2Vec2] = {
    input.map(line => {
      val spl = line.split(" ").toList
      spl match {
        case "up" :: amt :: _ => new D2Vec2(0, -amt.toInt)
        case "down" :: amt :: _ => new D2Vec2(0, amt.toInt)
        case "forward" :: amt :: _ => new D2Vec2(amt.toInt, 0)
      } 
    })
  }

  override def part1(input: List[D2Vec2]): Unit = {
    val finalDir = input.reduce(_ + _)
    logInfo("Final Direction = " + finalDir.toString)
    logInfo("Answer = " + finalDir.mul.toString)
  }

  override def part2(input: List[D2Vec2]): Unit = {
    val finalDir = input.foldLeft(D2Vec3()){
      (acc, dir) => acc + dir
    }
    logInfo("Final Direction = " + finalDir.toString)
    logInfo("Answer = " + finalDir.mul.toString)
  }

}
