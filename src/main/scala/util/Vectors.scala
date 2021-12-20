package util

trait BaseVec2 {
    def x: Int;
    def y: Int;
    def +(that: BaseVec2): BaseVec2;
}

case class Vec2(x: Int, y: Int) extends BaseVec2 {
  def +(that: BaseVec2): Vec2 = Vec2(x + that.x, y + that.y)
  def -(that: BaseVec2): Vec2 = Vec2(x - that.x, y - that.y)
}

trait BaseVec3 {
    def x: Int;
    def y: Int;
    def z: Int;
}

object BaseVec3 {
  implicit def ordering[A <: Vec3]: Ordering[Vec3] =
    Ordering.by(v => (v.x, v.y, v.z))
}

case class Vec3(x: Int, y: Int, z: Int) extends BaseVec3 {
  def +(that: BaseVec3): Vec3 = Vec3(x + that.x, y + that.y, z + that.z)
  def -(that: BaseVec3): Vec3 = Vec3(x - that.x, y - that.y, z - that.z)
  def mag(): Double = math.sqrt(math.pow(x, 2) + math.pow(y, 2) + math.pow(z, 2))
  def abs(): Vec3 = Vec3(math.abs(x), math.abs(y), math.abs(z))
  def >(other: Vec3): Boolean = List(this, other).sorted.head == other
  def *(v: Int): Vec3 = Vec3(v * x, v * y, v * z)

  def rotateX() = Vec3(x, z, -y)
  def rotateY() = Vec3(z, y, -x)
  def rotateZ() = Vec3(y, -x, z)
}