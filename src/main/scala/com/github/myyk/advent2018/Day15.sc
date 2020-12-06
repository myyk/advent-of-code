import com.github.myyk.advent2018._

import scala.util.control.Breaks

//val fileSource = false
val fileSource = true
val rawInput = if (fileSource) {
  readInput(15)
} else {
  ???
}

sealed trait Tile

case object Wall extends Tile

case object Open extends Tile

abstract class Creature(attackPower: Int, hp: Int = 200) extends Tile {
  def isDead(): Boolean = {
    hp <= 0
  }

  def isGoblin(): Boolean
}

case class Goblin(attackPower: Int = 3, hp: Int = 200) extends Creature(attackPower, hp) {
  def isGoblin(): Boolean = true
}

case class Elf(attackPower: Int = 3, hp: Int = 200) extends Creature(attackPower, hp) {
  def isGoblin(): Boolean = false
}

case class Location(x: Int, y: Int) {
  def up: Location = {
    this.copy(y = y - 1)
  }

  def down: Location = {
    this.copy(y = y + 1)
  }

  def right: Location = {
    this.copy(x = x + 1)
  }

  def left: Location = {
    this.copy(x = x - 1)
  }
}

class Dungeon(dungeonMatrix: Seq[Seq[Tile]], round: Int = 0) {
  def playRounds(): Unit = {
    while (playRound()) {}
  }

  // returns true if the battle is not over yet
  def playRound(): Boolean = {
    val creatureLocations = getCreatureLocations()
    for {
      (creature, location) <- creatureLocations
    } {
      if (!takeTurn(creature, location)) {
        return false
      }
    }

    // increment rounds complete
    round += 1
    true
  }

  // returns true if the battle is not over yet
  def takeTurn(creature: Creature, location: Location): Boolean = {
    if (isFinished()) {
      // check if combat is over - don't count this round if so.
      false
    } else if (!creature.isDead()) {
      // I assume the creature doesn't move if dead before it's turn?

      if (nextToEnemy(creature,location).isDefined) {
        // move
        //   find closest target
        ???
      }

      // attack
      for {
        closestEnemy <- nextToEnemy(creature,location)
      } {
        //   attack closest
        ???
      }
      true
    }
  }

  def getCreatureLocations(): Seq[(Creature, Location)] = {
    for {
      (row, y) <- dungeonMatrix.zipWithIndex
      (tile, x) <- row.zipWithIndex
      creature <- asCreature(tile)
    } yield {
      (creature, Location(x, y))
    }
  }

  def getCreatures(): Seq[Creature] = {
    getCreatureLocations.map(_._1)
  }

  def asCreature(tile: Tile): Option[Creature] = {
    tile match {
      case g: Goblin => Some(g)
      case e: Elf => Some(e)
      case _ => None
    }
  }

  def isFinished(): Boolean = {
    val creatures = getCreatures()
    val bothExist = creatures.exists(_.isGoblin()) && creatures.exists(!_.isGoblin())
    !bothExist
  }

  def nextToEnemy(creature: Creature, location: Location): Option[Creature] = {
    neighbors(location).find(_.isGoblin() != creature.isGoblin())
  }

  def neighborLocations(location: Location): Seq[Location] = {
    Seq(
      Location(location.y-1, location.x),
      Location(location.y, location.x-1),
      Location(location.y, location.x+1),
      Location(location.y+1, location.x),
    ).filterNot{location =>
      location.x<0 ||
        location.y <0 ||
        location.y >= dungeonMatrix.length ||
        location.x >= dungeonMatrix.head.length
    }
  }

  def neighbors(location: Location): Seq[Creature] = {
    for {
      neighborLocation <- neighborLocations(location)
      tile = dungeonMatrix(neighborLocation.y)(neighborLocation.x)
      creature <- asCreature(tile)
    } yield {
      creature
    }
  }

  def prettyPrint(): Unit = {
    for {
      row <- dungeonMatrix
    } {
      println("")
      for {
        tile <- row
      } {
        printTile(tile)
      }
    }
  }

  def printTile(tile: Tile): Unit = {
    val char = tile match {
      case _: Goblin => 'G'
      case _: Elf => 'E'
      case Wall => '#'
      case Open => '.'
    }
    print(char)
  }
}

val dungeon = new Dungeon(for {
  row <- rawInput
} yield {
  for {
    next <- row
  } yield {
    next match {
      case '#' => Wall
      case '.' => Open
      case 'G' => Goblin()
      case 'E' => Elf()
    }
  }
})

dungeon.prettyPrint()

// Answer 1
val answer1 = ???

// Answer 2
val answer2 = ???
