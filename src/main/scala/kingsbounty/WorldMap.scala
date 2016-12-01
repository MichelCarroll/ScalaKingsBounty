package kingsbounty


object WorldMap {

  val stringRepresentation =
    """~~~~%%%.-------.%%.....
      |~~~~%%..---=---.....%%%
      |~~~~~..^^--.--^^..%%%%%
      |~~~~~~.^^--.--^^..%%%%%
      |~~~.~~...........~~..%%
      |~~~.~~~~.~~~t.s..~~....
      |~~~~~~~~~~~~~~~~~~~~~~~
      |~~~~~~~~~~~~~~~~~~~~~~~
      |~~~~~~~~~~~~~~~~~~~~~~~""".stripMargin

  def characterToTile(character: Char): Option[Tile] = character match {
    case '.' => Some(Grass())
    case '~' => Some(Water())
    case '=' => Some(Castle("King Maximus", CastleRecruitmentPool(Set(Militia(), Archer(), Pikeman(), Cavalry(), Knight()))))
    case '-' => Some(Wall())
    case 't' => Some(Town())
    case 's' => Some(Sign())
    case '^' => Some(Forest())
    case '%' => Some(Mountain())
    case _ => None
  }

  val tiles: Map[Coordinate, Tile] = stringRepresentation
    .split('\n')
    .reverse //so that Y axis starts from the bottom
    .map(_.stripLineEnd)
    .map(_.stripMargin)
    .map(_.toCharArray)
    .zipWithIndex
    .flatMap(zippedRows => {
      val row = zippedRows._2
      zippedRows._1
        .zipWithIndex
        .flatMap(zippedCols => {
          val col = zippedCols._2
          characterToTile(zippedCols._1).map(tile => Coordinate(col, row) -> tile)
        })
    })
    .toMap


}