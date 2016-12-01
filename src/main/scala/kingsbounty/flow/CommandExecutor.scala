package kingsbounty.flow

import kingsbounty.character._
import kingsbounty.common._
import kingsbounty.tile.Castle
import kingsbounty.unit.UnitType


object CommandExecutor {

  object KeyCodes {
    val left = 37
    val up = 38
    val right = 39
    val down = 40
    val enter = 13
    val escape = 27
    val backspace = 8
    val a = 65
    val b = 66
    val c = 67
    val d = 68
    val e = 69
    val zero = 48
    val one = 49
    val two = 50
    val three = 51
    val four = 52
    val five = 53
    val six = 54
    val seven = 55
    val eight = 56
    val nine = 57
  }

  def executeCommand(currentInterface: UserInterface, keyCode: Int): UserInterface = {
    currentInterface match {

      case Loading() => currentInterface

      case Banner() => CharacterSelection(Paladin())

      case CharacterSelection(currentSelection) => currentSelection match {
        case Warrior() => keyCode match {
          case KeyCodes.left => CharacterSelection(Sorceress())
          case KeyCodes.up => CharacterSelection(Barbarian())
          case KeyCodes.enter => DifficultySelection(Warrior(), Normal())
          case _ => currentInterface
        }
        case Barbarian() => keyCode match {
          case KeyCodes.down => CharacterSelection(Warrior())
          case KeyCodes.left => CharacterSelection(Paladin())
          case KeyCodes.enter => DifficultySelection(Barbarian(), Normal())
          case _ => currentInterface
        }
        case Sorceress() => keyCode match {
          case KeyCodes.up => CharacterSelection(Paladin())
          case KeyCodes.right => CharacterSelection(Warrior())
          case KeyCodes.enter => DifficultySelection(Sorceress(), Normal())
          case _ => currentInterface
        }
        case Paladin() => keyCode match {
          case KeyCodes.right => CharacterSelection(Barbarian())
          case KeyCodes.down => CharacterSelection(Sorceress())
          case KeyCodes.enter => DifficultySelection(Paladin(), Normal())
          case _ => currentInterface
        }
      }

      case DifficultySelection(selectedClass, selectedDiffculty) => selectedDiffculty match {
        case Easy() => keyCode match {
          case KeyCodes.down => DifficultySelection(selectedClass, Normal())
          case KeyCodes.enter => Overworld(GameState.starting(selectedClass, selectedDiffculty))
          case _ => currentInterface
        }
        case Normal() => keyCode match {
          case KeyCodes.up => DifficultySelection(selectedClass, Easy())
          case KeyCodes.down => DifficultySelection(selectedClass, Hard())
          case KeyCodes.enter => Overworld(GameState.starting(selectedClass, selectedDiffculty))
          case _ => currentInterface
        }
        case Hard() => keyCode match {
          case KeyCodes.up => DifficultySelection(selectedClass, Normal())
          case KeyCodes.down => DifficultySelection(selectedClass, Impossible())
          case KeyCodes.enter => Overworld(GameState.starting(selectedClass, selectedDiffculty))
          case _ => currentInterface
        }
        case Impossible() => keyCode match {
          case KeyCodes.up => DifficultySelection(selectedClass, Hard())
          case KeyCodes.enter => Overworld(GameState.starting(selectedClass, selectedDiffculty))
          case _ => currentInterface
        }
      }

      case Overworld(gameState) => {

        def fromAttemptedMove(attemptedCoordinates: Coordinate): UserInterface =
          WorldMap.tiles.get(attemptedCoordinates) match {
            case Some(tile) =>
              if (tile.walkable)
                tile match {
                  case castleTile: Castle => CastleActionSelection(castleTile, gameState.copy(playerPosition = attemptedCoordinates))
                  case _ => Overworld(gameState.copy(playerPosition = attemptedCoordinates))
                }

              else Overworld(gameState)
            case None => Overworld(gameState)
          }

        keyCode match {
          case KeyCodes.up => fromAttemptedMove(gameState.playerPosition.up)
          case KeyCodes.down => fromAttemptedMove(gameState.playerPosition.down)
          case KeyCodes.right => fromAttemptedMove(gameState.playerPosition.right)
          case KeyCodes.left => fromAttemptedMove(gameState.playerPosition.left)
          case KeyCodes.a => ViewArmy(gameState)
          case _ => currentInterface
        }
      }

      case CastleActionSelection(castle, gameState) => keyCode match {
        case KeyCodes.escape => Overworld(gameState)
        case KeyCodes.a => CastleRecruitmentSelection(castle, gameState)
        case _ => currentInterface
      }

      case CastleRecruitmentSelection(castle, gameState) => {

        def fromUnitChoice(unit: UnitType): UserInterface = {
          val maxAmount = unit.max(gameState.playerGold)
          if(maxAmount > 0)
            CastleRecruitmentAmountSelection(castle, unit, gameState, NumberInputContext("", maxAmount))
          else
            CastleRecruitmentSelection(castle, gameState)
        }

        keyCode match {
          case KeyCodes.escape => CastleActionSelection(castle, gameState)
          case KeyCodes.a => fromUnitChoice(castle.recruitmentPool.a)
          case KeyCodes.b => fromUnitChoice(castle.recruitmentPool.b)
          case KeyCodes.c => fromUnitChoice(castle.recruitmentPool.c)
          case KeyCodes.d => fromUnitChoice(castle.recruitmentPool.d)
          case KeyCodes.e => fromUnitChoice(castle.recruitmentPool.e)
          case _ => currentInterface
        }
      }

      case CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext) => keyCode match {
        case KeyCodes.escape => CastleRecruitmentSelection(castle, gameState)
        case KeyCodes.backspace => CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext.delete)
        case KeyCodes.zero => CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext.append('0'))
        case KeyCodes.one => CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext.append('1'))
        case KeyCodes.two => CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext.append('2'))
        case KeyCodes.three => CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext.append('3'))
        case KeyCodes.four => CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext.append('4'))
        case KeyCodes.five => CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext.append('5'))
        case KeyCodes.six => CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext.append('6'))
        case KeyCodes.seven => CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext.append('7'))
        case KeyCodes.eight => CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext.append('8'))
        case KeyCodes.nine => CastleRecruitmentAmountSelection(castle, unit, gameState, numberInputContext.append('9'))
        case KeyCodes.enter => CastleRecruitmentSelection(castle, gameState.recruit(unit, numberInputContext.value))
        case _ => currentInterface
      }

      case ViewArmy(gameState) => keyCode match {
        case KeyCodes.escape => Overworld(gameState)
        case _ => currentInterface
      }
    }
  }
}
