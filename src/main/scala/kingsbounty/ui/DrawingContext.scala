package kingsbounty.ui

import kingsbounty.character._
import kingsbounty.common.{Coordinate, Gold, _}
import kingsbounty.flow.{CastleRecruitmentAmountSelection, CastleRecruitmentSelection, ViewArmy, _}
import kingsbounty.tile.{Forest, Mountain, Town, Wall, _}
import kingsbounty.unit.{Knight, _}
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLImageElement

class DrawingContext(canvasContext: dom.CanvasRenderingContext2D) {

  val imageRepository = new ImageRepository(canvasContext)
  val width = 600
  val height = 400
  val defaultFont = "16px Arial"

  canvasContext.canvas.width = width
  canvasContext.canvas.height = height

  private def clear(color: String) = {
    canvasContext.fillStyle = color
    canvasContext.fillRect(0, 0, width, height)
  }

  private def drawBannerScreen() = {
    clear("black")
    canvasContext.drawImage(imageRepository.banner, 175, 285, 1200, 700, 0, 0, width, height)

    canvasContext.textAlign = "left"
    canvasContext.font = "12px Arial"
    canvasContext.strokeText("Programming and Art by Michel Carroll", 10, 20)

    canvasContext.textAlign = "center"
    canvasContext.font = defaultFont
    canvasContext.strokeText("Press Any Key", width / 2 , height / 10 * 9)
  }

  private def drawCharacterSelectionScreen(selectedClass: CharacterClass) = {
    clear("black")

    canvasContext.textBaseline = "middle"
    canvasContext.textAlign = "center"
    canvasContext.font = defaultFont

    def drawCharacterChoice(imageSource: HTMLImageElement, text: String, x: Int, y: Int): Unit = {
      canvasContext.drawImage(imageSource,  width / 2 * x, height / 2 * y, width / 2, height / 2)
      canvasContext.strokeText(text, width / 4 * (x*2+1), height / 4 * (y*2+1))
    }

    Set(Warrior(), Paladin(), Sorceress(), Barbarian()).foreach {
      case Paladin() => drawCharacterChoice(imageRepository.paladin, "Paladin", 0, 0)
      case Barbarian() => drawCharacterChoice(imageRepository.barbarian, "Barbarian", 1, 0)
      case Sorceress() => drawCharacterChoice(imageRepository.sorceress, "Sorceress", 0, 1)
      case Warrior() => drawCharacterChoice(imageRepository.warrior, "Warrior", 1, 1)
    }

    def drawSelectionBox(text: String, x: Int, y: Int): Unit = {
      val textWidth = canvasContext.measureText(text).width
      val boxPadding = 5
      val estimatedTextHeight = 18
      canvasContext.strokeRect(
        (width / 4 * (x*2+1)) - textWidth / 2 - boxPadding,
        (height / 4 * (y*2+1)) - estimatedTextHeight / 2 - boxPadding,
        textWidth + boxPadding * 2,
        estimatedTextHeight + boxPadding * 2
      )
    }

    selectedClass match {
      case Paladin() => drawSelectionBox("Paladin", 0, 0)
      case Barbarian() => drawSelectionBox("Barbarian", 1, 0)
      case Sorceress() => drawSelectionBox("Sorceress", 0, 1)
      case Warrior() => drawSelectionBox("Warrior", 1, 1)
    }

  }

  private def drawDifficultySelectionScreen(selectedDiffculty: Difficulty) = {
    clear("white")

    canvasContext.textBaseline = "middle"
    canvasContext.textAlign = "center"
    canvasContext.font = defaultFont

    canvasContext.strokeText("Easy", width / 2, height / 8 * 1)
    canvasContext.strokeText("Normal", width / 2, height / 8 * 3)
    canvasContext.strokeText("Hard", width / 2, height / 8 * 5)
    canvasContext.strokeText("Impossible", width / 2, height / 8 * 7)

    def drawBox(text: String, i: Int): Unit = {
      val textWidth = canvasContext.measureText(text).width
      val boxPadding = 5
      val estimatedTextHeight = 18
      canvasContext.strokeRect(
        width / 2 - textWidth / 2 - boxPadding,
        height / 8 * (i*2+1) - estimatedTextHeight / 2 - boxPadding,
        textWidth + boxPadding * 2,
        estimatedTextHeight + boxPadding * 2
      )
    }

    selectedDiffculty match {
      case Easy() => drawBox("Easy", 0)
      case Normal() => drawBox("Normal", 1)
      case Hard() => drawBox("Hard", 2)
      case Impossible() => drawBox("Impossible", 3)
    }
  }

  private def tileToImage(tile: Tile): HTMLImageElement = tile match {
    case Grass() => imageRepository.grass
    case Water() => imageRepository.water
    case Sign() => imageRepository.sign
    case Castle(_,_) => imageRepository.castle
    case Wall() => imageRepository.wall
    case Town() => imageRepository.town
    case Forest() => imageRepository.forest
    case Mountain() => imageRepository.mountain
  }

  private def unitToImage(unit: UnitType): HTMLImageElement = unit match {
    case Militia() => imageRepository.militia
    case Archer() => imageRepository.archer
    case Pikeman() => imageRepository.pikeman
    case Cavalry() => imageRepository.cavalier
    case Knight() => imageRepository.knight
  }

  private def drawViewArmy(gameState: GameState): Unit = {
    clear("white")
    val textPadding = 10
    val heightPerUnit = (height - 16) / 5
    val imageHeight = heightPerUnit
    val imageWidth = heightPerUnit
    val heightPerTextLine = heightPerUnit / 3
    val widthPerTextLine = (width - imageWidth - textPadding * 2) / 2

    canvasContext.textBaseline = "middle"
    canvasContext.textAlign = "left"
    canvasContext.font = defaultFont

    gameState.playerArmy.unitsSortedByCost.zipWithIndex.foreach(zippedArmyUnit => {
      val heightOffset = heightPerUnit * zippedArmyUnit._2 + 36

      canvasContext.strokeText(
        s"${zippedArmyUnit._1.amount} ${zippedArmyUnit._1.unitType.pluralTitle}",
        imageHeight + textPadding,
        heightOffset + heightPerUnit / 2 - heightPerTextLine
      )

      canvasContext.strokeText(
        s"Skill Level: ${zippedArmyUnit._1.skillLevel.level}",
        imageHeight + textPadding,
        heightOffset + heightPerUnit / 2
      )

      canvasContext.strokeText(
        s"Movement Points: ${zippedArmyUnit._1.movementPoints.points}",
        imageHeight + textPadding,
        heightOffset + heightPerUnit / 2 + heightPerTextLine
      )

      canvasContext.strokeText(
        s"Hit Points: ${zippedArmyUnit._1.maxHitPoints.points}",
        imageHeight + textPadding + widthPerTextLine,
        heightOffset + heightPerUnit / 2 - heightPerTextLine
      )

      canvasContext.strokeText(
        s"Melee Damage: ${zippedArmyUnit._1.meleeDamageRange.min}-${zippedArmyUnit._1.meleeDamageRange.max}",
        imageHeight + textPadding + widthPerTextLine,
        heightOffset + heightPerUnit / 2
      )

      zippedArmyUnit._1.rangedDamageRange match {
        case Some(rangedDamageRange) =>
          canvasContext.strokeText(
            s"Ranged Damage: ${rangedDamageRange.min}-${rangedDamageRange.max}",
            imageHeight + textPadding + widthPerTextLine,
            heightOffset + heightPerUnit / 2 + heightPerTextLine
          )
        case None =>
      }

      canvasContext.drawImage(
        unitToImage(zippedArmyUnit._1.unitType),
        0,
        heightOffset,
        imageWidth,
        imageHeight
      )
    })
    escapeToLeave()
  }

  val viewWidthInTiles = 5
  val viewHeightInTiles = 5
  val tileWidth = width / viewWidthInTiles
  val tileHeight = height / viewHeightInTiles

  private def drawTiles(gameState: GameState): Unit = {

    for {
      x <- 0 until 5
      y <- 0 until 5
    } {
      val absX = gameState.playerPosition.x + x - 2
      val absY = gameState.playerPosition.y + y - 2

      WorldMap.tiles.get(Coordinate(absX, absY)) match {
        case Some(tile) =>
          canvasContext.drawImage(
            tileToImage(tile),
            x * tileWidth,
            (viewHeightInTiles - 1 - y) * tileHeight,
            tileWidth,
            tileHeight
          )
        case None =>
      }
    }
  }

  private def drawPlayer(gameState: GameState): Unit = {
    canvasContext.drawImage(
      imageRepository.player,
      tileWidth * Math.floor(viewWidthInTiles / 2),
      tileHeight * Math.floor(viewHeightInTiles / 2),
      tileWidth,
      tileHeight
    )
  }

  private def drawOverworld(gameState: GameState): Unit = {
    clear("white")
    drawTiles(gameState)
    drawPlayer(gameState)
  }

  private def locationOptions(description: String, choices: List[String]): Unit = {
    canvasContext.textBaseline = "alphabetic"
    canvasContext.textAlign = "left"
    canvasContext.font = defaultFont

    canvasContext.strokeText(description, width / 8, height / 2)
    choices.zipWithIndex.foreach(zippedChoice =>
      canvasContext.strokeText(zippedChoice._1, width / 8, height / 2 + 32 + (zippedChoice._2 * 16))
    )
  }

  private def escapeToLeave(): Unit = {
    canvasContext.textBaseline = "middle"
    canvasContext.textAlign = "center"
    canvasContext.font = defaultFont

    canvasContext.strokeText("Press ESC to leave", width / 2, 20)
  }

  private def drawOptionsMoneyAmount(gold: Gold): Unit = {
    canvasContext.textBaseline = "alphabetic"
    canvasContext.textAlign = "right"
    canvasContext.font = defaultFont

    canvasContext.strokeText(s"Gold: ${gold.amount}", width / 8 * 7, height / 2)
  }


  private def drawCastleActionSelection(castle: Castle): Unit = {
    clear("white")
    locationOptions(s"Castle of ${castle.kingName}", List(
      "A) Recruit Soldiers",
      "B) Audience with the King"
    ))

    escapeToLeave()
  }

  private def drawCastleRecruitmentSelection(castle: Castle, playerGold: Gold): Unit = {
    clear("white")
    locationOptions(
      s"Recruit Soldiers",
      castle.recruitmentPool.alphabeticallyKeyed.map(x =>
        s"${x._1}) ${x._2.pluralTitle} (${x._2.cost.amount})"
      )
    )
    drawOptionsMoneyAmount(playerGold)

    escapeToLeave()
  }

  private def drawNumberInput(max: Int, numberInputContext: NumberInputContext): Unit = {
    canvasContext.textBaseline = "alphabetic"
    canvasContext.textAlign = "right"
    canvasContext.font = defaultFont
    canvasContext.strokeText(s"Max: $max", width / 8 * 7, height / 3 * 2)
    canvasContext.strokeText(s"How Many", width / 8 * 7, height / 3 * 2 + 16)
    canvasContext.strokeText(numberInputContext.numberString, width / 8 * 7, height / 3 * 2 + 32)
  }

  private def drawCastleRecruitmentAmountSelection(castle: Castle, gameUnit: UnitType, playerGold: Gold, numberInputContext: NumberInputContext): Unit = {
    drawCastleRecruitmentSelection(castle, playerGold)
    drawNumberInput(gameUnit.max(playerGold), numberInputContext)
  }

  def draw(currentInterface: UserInterface) = {
    currentInterface match {

      case Loading() =>

      case Banner() =>
        drawBannerScreen()

      case CharacterSelection(selectedClass) =>
        drawCharacterSelectionScreen(selectedClass)

      case DifficultySelection(_, selectedDiffculty) =>
        drawDifficultySelectionScreen(selectedDiffculty)

      case Overworld(gameContext) =>
        drawOverworld(gameContext)

      case CastleActionSelection(castle, gameContext) =>
        drawCastleActionSelection(castle)

      case CastleRecruitmentSelection(castle, gameContext) =>
        drawCastleRecruitmentSelection(castle, gameContext.playerGold)

      case CastleRecruitmentAmountSelection(castle, unit, gameContext, numberInputContext) =>
        drawCastleRecruitmentAmountSelection(castle, unit, gameContext.playerGold, numberInputContext)

      case ViewArmy(gameState) =>
        drawViewArmy(gameState)
    }
  }

  def ready = imageRepository.loaded

}