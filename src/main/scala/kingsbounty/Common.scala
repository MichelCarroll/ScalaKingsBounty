package kingsbounty
import math.floor

case class Coordinate(x: Int, y: Int) {
  def right = Coordinate(x + 1, y)
  def left = Coordinate(x - 1, y)
  def up = Coordinate(x, y + 1)
  def down = Coordinate(x, y - 1)
}

sealed trait Tile {
  def walkable: Boolean
}

case class Grass() extends Tile {
  def walkable = true
}

case class Water() extends Tile {
  def walkable = false
}

case class Castle(kingName: String, recruitmentPool: CastleRecruitmentPool) extends Tile {
  def walkable = true
}

case class Wall() extends Tile {
  def walkable = false
}

case class Mountain() extends Tile {
  def walkable = false
}

case class Sign() extends Tile {
  def walkable = true
}

case class Town() extends Tile {
  def walkable = true
}

case class Forest() extends Tile {
  def walkable = false
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

case class ArmyUnit(unitType: UnitType, amount: Int)  {
  def skillLevel = unitType.skillLevel
  def maxHitPoints = unitType.maxHitPoints * amount
  def movementPoints = unitType.movementPoints
  def meleeDamageRange = unitType.meleeDamageRange * amount
  def rangedDamageRange = unitType.rangedDamageRange.map(_ * amount)
}

case class Army(units: Set[ArmyUnit]) {
  assert(units.size <= 5)
  def isFull = units.size == 5
  def isEmpty = units.isEmpty

  def unitsSortedByCost: List[ArmyUnit] = units.toList.sortBy(_.unitType.cost.amount)

  def append(unitType: UnitType, amount: Int) =
    if(units.exists(_.unitType == unitType)) {
      Army(units.map(armyUnit =>
        if(armyUnit.unitType == unitType)
          armyUnit.copy(amount = armyUnit.amount + amount)
        else armyUnit
      ))
    }
    else
      Army(units + ArmyUnit(unitType, amount))

}

case class CastleRecruitmentPool(units: Set[UnitType]) {
  assert(units.size == 5)

  def unitsSortedByCost: List[UnitType] = units.toList.sortBy(_.cost.amount)
  def alphabeticallyKeyed = List("A", "B", "C", "D", "E").zip(unitsSortedByCost)

  def a = alphabeticallyKeyed.filter(_._1 == "A").head._2
  def b = alphabeticallyKeyed.filter(_._1 == "B").head._2
  def c = alphabeticallyKeyed.filter(_._1 == "C").head._2
  def d = alphabeticallyKeyed.filter(_._1 == "D").head._2
  def e = alphabeticallyKeyed.filter(_._1 == "E").head._2
}

case class Gold(amount: Int) extends AnyVal {
  def times(i: Int) = Gold(amount * i)
  def -(that: Gold) = Gold(amount - that.amount)
  def +(that: Gold) = Gold(amount + that.amount)
}

case class GameState(
                      characterClass: CharacterClass,
                      difficulty: Difficulty,
                      playerPosition: Coordinate,
                      playerArmy: Army,
                      playerGold: Gold
                    ) {

  def recruit(unit: UnitType, amount: Int) = this.copy(
    playerGold = playerGold - (unit.cost times amount),
    playerArmy = playerArmy.append(unit, amount)
  )

}

object GameState {
  def starting(characterClass: CharacterClass, difficulty: Difficulty): GameState =
    GameState(
      characterClass,
      difficulty,
      Coordinate(11,5),
      Army(Set(
        ArmyUnit(Militia(), 20),
        ArmyUnit(Archer(), 2)
      )),
      Gold(7000)
    )
}

sealed trait Difficulty
case class Easy() extends Difficulty
case class Normal() extends Difficulty
case class Hard() extends Difficulty
case class Impossible() extends Difficulty

sealed trait CharacterClass
case class Paladin() extends CharacterClass
case class Warrior() extends CharacterClass
case class Sorceress() extends CharacterClass
case class Barbarian() extends CharacterClass

case class NumberInputContext(numberString: String, max: Int) {
  def value =
    if(numberString.length == 0) 0
    else numberString.toInt

  def delete =
    if(numberString.length == 0) this
    else NumberInputContext(numberString.reverse.tail.reverse, max)

  def append(character: Char) = {
    val potentialNewValue = NumberInputContext(numberString + character, max)
    if(potentialNewValue.value > max) NumberInputContext(max.toString, max)
    else  potentialNewValue
  }
}

sealed trait UserInterface
case class Loading() extends UserInterface
case class Banner() extends UserInterface
case class CharacterSelection(selectedClass: CharacterClass) extends UserInterface
case class DifficultySelection(selectedClass: CharacterClass, selectedDiffculty: Difficulty) extends UserInterface
case class Overworld(gameState: GameState) extends UserInterface
case class CastleActionSelection(castle: Castle, gameState: GameState) extends UserInterface
case class CastleRecruitmentSelection(castle: Castle, gameState: GameState) extends UserInterface
case class CastleRecruitmentAmountSelection(castle: Castle, unit: UnitType, gameState: GameState, numberInputContext: NumberInputContext) extends UserInterface
case class ViewArmy(gameState: GameState) extends UserInterface