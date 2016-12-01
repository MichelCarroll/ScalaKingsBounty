package kingsbounty.character


sealed trait CharacterClass
case class Paladin() extends CharacterClass
case class Warrior() extends CharacterClass
case class Sorceress() extends CharacterClass
case class Barbarian() extends CharacterClass
