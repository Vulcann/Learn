package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  def initPersons(): List[Person] = {
    val persons = for {
      id <- (0 until SimConfig.population).toList
    } yield new Person(id)
    for {
      person <- persons
    } {
      if (person.id < 3)
        person.infected = true
        person.infect()
    }
    persons
  }
  //val persons: List[Person] = List() // to complete: construct list of persons
  val persons: List[Person] = initPersons()

  def initActions(): Unit = {
    persons map (_.touch())
  }
  initActions()

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    def getInfected() = {
      if (this.immune) false
      else {
        val n = randomBelow(5)
        if (n<=1) true
        else false
      }
    }
    def getDead() = {
      val n = randomBelow(4)
      if (n==0) true
      else false
    }
    // actions:
    def move(): Unit = {
      if (!this.dead) {
        val moveDir = randomBelow(4)
        if (moveDir == 0) {
          col += 1
          if (col==roomColumns) col = 0
        }
        else if (moveDir == 1) {
          row += 1
          if (row==roomRows) row = 0
        }
        else if (moveDir == 2) {
          col -= 1
          if (col<0) col = roomColumns-1
        }
        else {
          row -= 1
          if (row<0) row = roomRows-1
        }
        afterDelay(0) {
          touch()
        }
      }
    }
    def touch(): Unit = {
      if (!this.dead) {
        var infectedNum = 0;
        for {
          person <- persons
          if (person.row == this.row && person.col == this.col)
        } infectedNum += 1
        if (infectedNum > 0 && getInfected()) {
          infect()
        }
        val cooldown = 1 + randomBelow(5)
        afterDelay(cooldown) {
          move()
        }
      }
    }
    def infect(): Unit = {
      afterDelay(0) {
        this.infected = true
      }
      afterDelay(6) {
        this.sick = true
      }
      afterDelay(14) {
        if (getDead()) this.dead = true
      }
      afterDelay(16) {
        if (!this.dead) {
          this.immune = true
          this.sick = false
        }
      }
      afterDelay(18) {
        if (!this.dead) {
          this.infected = false
          this.sick = false
          this.immune = false
        }
      }
    }

    def performAction(): Unit = {

    }
  }

}
