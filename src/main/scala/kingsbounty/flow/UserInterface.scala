package kingsbounty.flow

import kingsbounty.tile.Castle
import kingsbounty.character.CharacterClass
import kingsbounty.common.Difficulty
import kingsbounty.unit.UnitType

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