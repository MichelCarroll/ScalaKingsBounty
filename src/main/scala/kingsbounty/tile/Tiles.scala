package kingsbounty.tile

import kingsbounty.unit.CastleRecruitmentPool


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
