package kingsbounty.unit

import kingsbounty.common._

import scala.math.floor


sealed trait UnitType {
  def cost: Gold
  def singularTitle: String
  def pluralTitle: String
  def unlocked: Boolean
  def moraleGroup: MoraleGroup
  def skillLevel: SkillLevel
  def maxHitPoints: HitPoints
  def movementPoints: MovementPoints
  def meleeDamageRange: DamageRange
  def rangedDamageRange: Option[DamageRange]

  def max(playerGold: Gold): Int =
    if(unlocked) floor(playerGold.amount / cost.amount).toInt
    else 0
}

case class Militia() extends UnitType {
  def cost = Gold(50)
  def singularTitle = "Militia"
  def pluralTitle = "Militia"
  def unlocked = true
  def skillLevel = SkillLevel(2)
  def maxHitPoints = HitPoints(2)
  def movementPoints = MovementPoints(2)
  def meleeDamageRange = DamageRange(1, 2)
  def rangedDamageRange = None
  def moraleGroup = MoraleGroup.A()
}
case class Archer() extends UnitType {
  def cost = Gold(250)
  def singularTitle = "Archer"
  def pluralTitle = "Archers"
  def unlocked = true
  def skillLevel = SkillLevel(2)
  def maxHitPoints = HitPoints(10)
  def movementPoints = MovementPoints(2)
  def meleeDamageRange = DamageRange(1, 2)
  def rangedDamageRange = Some(DamageRange(1, 3))
  def moraleGroup = MoraleGroup.B()
}
case class Pikeman() extends UnitType {
  def cost = Gold(300)
  def singularTitle = "Pikeman"
  def pluralTitle = "Pikemen"
  def unlocked = true
  def skillLevel = SkillLevel(3)
  def maxHitPoints = HitPoints(10)
  def movementPoints = MovementPoints(2)
  def meleeDamageRange = DamageRange(2, 4)
  def rangedDamageRange = None
  def moraleGroup = MoraleGroup.B()
}
case class Cavalry() extends UnitType {
  def cost = Gold(800)
  def singularTitle = "Cavalry"
  def pluralTitle = "Cavalry"
  def unlocked = false
  def skillLevel = SkillLevel(4)
  def maxHitPoints = HitPoints(20)
  def movementPoints = MovementPoints(4)
  def meleeDamageRange = DamageRange(3, 5)
  def rangedDamageRange = None
  def moraleGroup = MoraleGroup.B()
}
case class Knight() extends UnitType {
  def cost = Gold(1000)
  def singularTitle = "Knight"
  def pluralTitle = "Knights"
  def unlocked = false
  def skillLevel = SkillLevel(5)
  def maxHitPoints = HitPoints(35)
  def movementPoints = MovementPoints(1)
  def meleeDamageRange = DamageRange(6, 10)
  def rangedDamageRange = None
  def moraleGroup = MoraleGroup.B()
}
