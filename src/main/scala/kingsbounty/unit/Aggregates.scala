package kingsbounty.unit

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