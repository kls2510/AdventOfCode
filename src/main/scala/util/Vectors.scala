package util

trait BaseVec2 {
    def x: Int;
    def y: Int;
    def +(that: BaseVec2): BaseVec2;
}

case class Vec2(x: Int, y: Int) extends BaseVec2 {
  def +(that: BaseVec2): Vec2 = Vec2(x + that.x, y + that.y)
}

trait BaseVec3 {
    def x: Int;
    def y: Int;
    def z: Int;
}

case class Vec3(x: Int, y: Int, z: Int) extends BaseVec3 {
  def +(that: BaseVec3): Vec3 = Vec3(x + that.x, y + that.y, z + that.z)
}