package kingsbounty.common

case class Coordinate(x: Int, y: Int) {
  def right = Coordinate(x + 1, y)
  def left = Coordinate(x - 1, y)
  def up = Coordinate(x, y + 1)
  def down = Coordinate(x, y - 1)
}

case class DamageRange(min: Int, max: Int) {
  assert(min <= max)

  def *(that: Int) = DamageRange(min * that, max * that)
}

sealed trait MoraleGroup
object MoraleGroup {
  case class A() extends MoraleGroup
  case class B() extends MoraleGroup
  case class C() extends MoraleGroup
  case class D() extends MoraleGroup
  case class E() extends MoraleGroup
}

case class SkillLevel(level: Int) extends AnyVal
case class MovementPoints(points: Int) extends AnyVal

case class HitPoints(points: Int) extends AnyVal {
  def *(that: Int) = HitPoints(points * that)
}

case class Gold(amount: Int) extends AnyVal {
  def times(i: Int) = Gold(amount * i)
  def -(that: Gold) = Gold(amount - that.amount)
  def +(that: Gold) = Gold(amount + that.amount)
}

sealed trait Difficulty
case class Easy() extends Difficulty
case class Normal() extends Difficulty
case class Hard() extends Difficulty
case class Impossible() extends Difficulty
